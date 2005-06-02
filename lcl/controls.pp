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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  Classes, SysUtils, TypInfo, FPCAdds, DynHashArray, LCLStrConsts, LCLType,
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
  TTranslateString = type String;
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
    csFixedHeight,
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
    csHasCancelAction   // control implements useful ExecuteCancelAction
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
    procedure FreeHandle;
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
  end;



  TKeyEvent = procedure(Sender: TObject; var Key: Word; Shift:TShiftState) of Object;
  TKeyPressEvent = procedure(Sender: TObject; var Key: char) of Object;
  TUTF8KeyPressEvent = procedure(Sender: TObject; var UTF8Key: TUTF8Char) of Object;

  TMouseEvent = Procedure(Sender: TOBject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer) of object;
  TMouseMoveEvent = Procedure(Sender: TObject; Shift: TShiftState;
                              X, Y: Integer) of object;
  TMouseWheelEvent = Procedure(Sender: TObject; Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of object;
  TMouseWheelUpDownEvent = Procedure(Sender: TObject;
          Shift: TShiftState; MousePos: TPoint; var Handled: Boolean) of object;


  { TDragObject }

  TDragObject = class;

  TDragState = (dsDragEnter, dsDragLeave, dsDragMove);
  TDragMode = (dmManual , dmAutomatic);
  TDragKind = (dkDrag, dkDock);
  TDragOperation = (
    dopNone,  // not dragging or Drag initialized, but not yet started.
              //  Waiting for mouse move more then Treshold.
    dopDrag,  // Dragging
    dopDock   // Docking
    );
  TDragMessage = (dmDragEnter, dmDragLeave, dmDragMove, dmDragDrop,
                  dmDragCancel,dmFindTarget);
  TDragOverEvent = Procedure(Sender, Source: TObject;
               X,Y: Integer; State: TDragState; Var Accept: Boolean) of Object;
  TDragDropEvent = Procedure(Sender, Source: TObject; X,Y: Integer) of Object;
  TStartDragEvent = Procedure(Sender: TObject; DragObject: TDragObject) of Object;
  TEndDragEvent = Procedure(Sender, Target: TObject; X,Y: Integer) of Object;


  PDragRec = ^TDragRec;
  TDragRec = record
    Pos: TPoint;
    Source: TDragObject;
    Target: TControl;
    Docking: Boolean;
  end;

  TCMDrag = packed record
    Msg: Cardinal;
    DragMessage: TDragMessage;
    Reserved1: Byte; // for Delphi compatibility
    Reserved2: Word; // for Delphi compatibility
    DragRec: PDragRec;
    Result: LRESULT;
  end;

  TDragObject = class(TObject)
  private
    FDragTarget: TControl;
    FDragHandle: HWND;
    FDragPos: TPoint;
    FDragTargetPos: TPoint;
    FDropped: Boolean;
    FMouseDeltaX: Double;
    FMouseDeltaY: Double;
    FCancelling: Boolean;
    function Capture: HWND;
  protected
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; virtual;
    function GetDragImages: TDragImageList; virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X, Y: Integer); virtual;
    procedure CaptureChanged(OldCaptureControl: TControl); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
  public
    destructor Destroy; override;
    procedure Assign(Source: TDragObject); virtual;
    function GetName: string; virtual;
    procedure HideDragImage; virtual;
    function Instance: THandle; virtual;
    procedure ShowDragImage; virtual;
    property Cancelling: Boolean read FCancelling write FCancelling;
    property DragHandle: HWND read FDragHandle write FDragHandle;
    property DragPos: TPoint read FDragPos write FDragPos;
    property DragTargetPos: TPoint read FDragTargetPos write FDragTargetPos;
    property DragTarget: TControl read FDragTarget write FDragTarget;
    property Dropped: Boolean read FDropped;
    property MouseDeltaX: Double read FMouseDeltaX;
    property MouseDeltaY: Double read FMouseDeltaX;
  end;

  TDragObjectClass = class of TDragObject;


  { TBaseDragControlObject }

  TBaseDragControlObject = class(TDragObject)
  private
    FControl: TControl;
  protected
    Procedure EndDrag(Target: TObject; X, Y: Integer); Virtual;
    procedure Finished(Target: TObject; X, Y: Integer; Accepted: Boolean); override;
  Public
    constructor Create(AControl: TControl); virtual;
    procedure Assign(Source: TDragObject); override;
    property Control: TControl read FControl write FControl;
  end;


  { TDragControlObject }

  TDragControlObject = class(TBaseDragControlObject)
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetDragImages: TDragImageList; override;
  public
    procedure HideDragImage; override;
    procedure ShowDragImage; override;
  end;


  { TDragDockObject }

  TDragDockObject = class;

  TDockOrientation = (
    doNoOrient,   // zone contains a TControl and no child zones.
    doHorizontal, // zone's children are stacked top-to-bottom.
    doVertical    // zone's children are arranged left-to-right.
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

  TDragDockObject = class(TBaseDragControlObject)
  private
    FBrush: TBrush;
    FDockRect: TRect;
    FDropAlign: TAlign;
    FDropOnControl: TControl;
    FFloating: Boolean;
    procedure SetBrush(Value: TBrush);
  protected
    procedure AdjustDockRect(ARect: TRect); virtual;
    procedure DrawDragDockImage; virtual;
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    procedure EraseDragDockImage; virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetFrameWidth: Integer; virtual;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
    procedure Assign(Source: TDragObject); override;
    property Brush: TBrush read FBrush write SetBrush;
    property DockRect: TRect read FDockRect write FDockRect;
    property DropAlign: TAlign read FDropAlign;
    property DropOnControl: TControl read FDropOnControl;
    property Floating: Boolean read FFloating write FFloating;
    property FrameWidth: Integer read GetFrameWidth;
  end;


  { TDockManager is an abstract class for managing a dock site's docked
    controls. See TDockTree for the default dock manager }
  TDockManager = class
    procedure BeginUpdate; virtual; abstract;
    procedure EndUpdate; virtual; abstract;
    procedure GetControlBounds(Control: TControl;
      var AControlBounds: TRect); virtual; abstract;
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

  TSizeConstraintsOption = (scoAdviceWidthAsMin, scoAdviceWidthAsMax,
    scoAdviceHeightAsMin, scoAdviceHeightAsMax);
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

  }
  
  TSpacingSize = 0..MaxInt;

  { TControlBorderSpacing }

  TControlBorderSpacing = class(TPersistent)
  private
    FAround: TSpacingSize;
    FBottom: TSpacingSize;
    FControl: TControl;
    FLeft: TSpacingSize;
    FOnChange: TNotifyEvent;
    FRight: TSpacingSize;
    FTop: TSpacingSize;
    procedure SetAround(const AValue: TSpacingSize);
    procedure SetBottom(const AValue: TSpacingSize);
    procedure SetLeft(const AValue: TSpacingSize);
    procedure SetRight(const AValue: TSpacingSize);
    procedure SetSpace(Kind: TAnchorKind; const AValue: integer);
    procedure SetTop(const AValue: TSpacingSize);
  protected
    procedure Change; dynamic;
  public
    constructor Create(OwnerControl: TControl);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function IsEqual(Spacing: TControlBorderSpacing): boolean;
    procedure GetSpaceAround(var SpaceAround: TRect);
    function GetSpace(Kind: TAnchorKind): Integer;
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
    kept distance is defined by the BorderSpacing.
    
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
    procedure GetSidePosition(var ReferenceControl: TControl;
                var ReferenceSide: TAnchorSideReference; var Position: Integer);
    procedure Assign(Source: TPersistent); override;
  public
    property Owner: TControl read FOwner;
    property Kind: TAnchorKind read FKind;
  published
    property Control: TControl read FControl write SetControl;
    property Side: TAnchorSideReference read FSide write SetSide stored IsSideStored;
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
    cfClientWidthLoaded,
    cfClientHeightLoaded,
    cfLastAlignedBoundsValid,
    cfBoundsRectForNewParentValid,
    cfPreferredSizeValid
    );
  TControlFlags = set of TControlFlag;

  TControlHandlerType = (
    chtOnResize,
    chtOnChangeBounds
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
    FAutoSize: Boolean;
    FBaseBounds: TRect;
    FBaseBoundsLock: integer;
    FBaseParentClientSize: TPoint;
    FBorderSpacing: TControlBorderSpacing;
    FBoundsRectForNewParent: TRect;
    FCaption: TCaption;
    FColor: TColor;
    FConstraints: TSizeConstraints;
    FControlFlags: TControlFlags;
    FControlHandlers: array[TControlHandlerType] of TMethodList;
    FControlStyle: TControlStyle;
    FCtl3D: Boolean;
    FCursor: TCursor;
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
    FLastChangebounds: TRect;
    FLastDoChangeBounds: TRect;
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
    FPreferredWidth: integer;
    FPreferredHeight: integer;
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
    procedure DoActionChange(Sender: TObject);
    function GetAnchorSide(Kind: TAnchorKind): TAnchorSide;
    function GetAnchorSideIndex(Index: integer): TAnchorSide;
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
    function CheckMenuPopup(const P: TSmallPoint): boolean;
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
    procedure SetClientSize(Value: TPoint);
    procedure SetClientWidth(Value: Integer);
    procedure SetConstraints(const Value: TSizeConstraints);
    procedure SetCursor(Value: TCursor);
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
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetShowHint(Value: Boolean);
    procedure SetText(const Value: TCaption); 
    procedure SetTop(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
  protected
    FControlState: TControlState;
  protected
    // sizing/aligning
    AutoSizing: Boolean;
    procedure DoAutoSize; virtual;
    function AutoSizeCanStart: boolean; virtual;
    procedure AnchorSideChanged(TheAnchorSide: TAnchorSide); virtual;
    procedure SetAlign(Value: TAlign); virtual;
    procedure SetAnchors(const AValue: TAnchors); virtual;
    procedure SetAutoSize(const Value: Boolean); virtual;
    procedure BoundsChanged; dynamic;
    procedure DoConstraintsChange(Sender: TObject); virtual;
    procedure DoBorderSpacingChange(Sender: TObject); virtual;
    procedure SendMoveSizeMessages(SizeChanged, PosChanged: boolean); virtual;
    procedure ConstrainedResize(var MinWidth, MinHeight,
                                MaxWidth, MaxHeight: TConstraintSize); virtual;
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: integer); virtual;
    procedure DoOnResize; virtual;
    procedure DoOnChangeBounds; virtual;
    procedure Resize; virtual;
    procedure RequestAlign; dynamic;
    procedure UpdateBaseBounds(StoreBounds, StoreParentClientSize,
                               UseLoadedValues: boolean); virtual;
    procedure LockBaseBounds;
    procedure UnlockBaseBounds;
    procedure UpdateAnchorRules;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer); virtual;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); virtual;
    procedure ChangeScale(Multiplier, Divider: Integer); dynamic;
    Function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; virtual;
    procedure SetAlignedBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    Function GetClientOrigin: TPoint; virtual;
    Function GetClientRect: TRect; virtual;
    Function GetScrolledClientRect: TRect; virtual;
    function GetChildsRect(Scrolled: boolean): TRect; virtual;
    function GetClientScrollOffset: TPoint; virtual;
    function GetControlOrigin: TPoint; virtual;
  protected
    // protected messages
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
    procedure WMDragStart(Var Message: TLMessage); message LM_DRAGSTART;//not in delphi
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
  protected
    // drag and drop
    procedure CalculateDockSizes;
    function CreateFloatingDockSite(const Bounds: TRect): TWinControl;
    function GetDockEdge(const MousePos: TPoint): TAlign; dynamic;
    function GetFloating: Boolean; virtual;
    function GetFloatingDockSiteClass: TWinControlClass; virtual;
    procedure BeginAutoDrag; dynamic;
    procedure DefaultDockImage(DragDockObject: TDragDockObject; Erase: Boolean); dynamic;
    procedure DockTrackNoTarget(Source: TDragDockObject; X, Y: Integer); dynamic;
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); dynamic;
    procedure DoDragMsg(var DragMsg: TCMDrag); virtual;
    procedure DoEndDock(Target: TObject; X, Y: Integer); dynamic;
    procedure DoEndDrag(Target: TObject; X,Y: Integer); dynamic;
    procedure DoStartDock(var DragObject: TDragObject); dynamic;
    procedure DoStartDrag(var DragObject: TDragObject); dynamic;
    procedure DragCanceled; dynamic;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState;
                       var Accept: Boolean); dynamic;
    procedure DrawDragDockImage(DragDockObject: TDragDockObject); dynamic;
    procedure EraseDragDockImage(DragDockObject: TDragDockObject); dynamic;
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
  protected
    procedure Changed;
    function  GetPalette: HPalette; virtual;
    function ChildClassAllowed(ChildClass: TClass): boolean; virtual;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure InvalidateControl(CtrlIsVisible, CtrlIsOpaque: Boolean);
    procedure InvalidateControl(CtrlIsVisible, CtrlIsOpaque, IgnoreWinControls: Boolean);
    procedure FontChanged(Sender: TObject); virtual;
    function GetAction: TBasicAction; virtual;
    function RealGetText: TCaption; virtual;
    procedure RealSetText(const Value: TCaption); virtual;
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
    procedure AddHandler(HandlerType: TControlHandlerType;
                         const AMethod: TMethod; AsLast: boolean);
    procedure RemoveHandler(HandlerType: TControlHandlerType;
                            const AMethod: TMethod);
    procedure DoContextPopup(const MousePos: TPoint; var Handled: Boolean); virtual;
  protected
    // actions
    function GetActionLinkClass: TControlActionLinkClass; dynamic;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); dynamic;
  protected
    // optional properties (not every descendent supports them)
    property ActionLink: TControlActionLink read FActionLink write FActionLink;
    property Ctl3D: Boolean read FCtl3D write FCtl3D;//Is this needed for anything other than compatability?
    property DragCursor: TCursor read FDragCursor write SetDragCursor default crDrag;
    property DragKind: TDragKind read FDragKind write FDragKind default dkDrag;
    property DragMode: TDragMode read fDragMode write SetDragMode default dmManual;
    property MouseCapture: Boolean read GetMouseCapture write SetMouseCapture;
    property ParentFont: Boolean  read FParentFont write FParentFont;
    property ParentColor: Boolean  read FParentColor write SetParentColor;
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
    Procedure DragDrop(Source: TObject; X,Y: Integer); Dynamic;
    procedure Dock(NewDockSite: TWinControl; ARect: TRect); dynamic;
    function ManualDock(NewDockSite: TWinControl;
      DropControl: TControl {$IFNDEF VER1_0}= nil{$ENDIF};
      ControlSide: TAlign {$IFNDEF VER1_0}= alNone{$ENDIF}): Boolean;
    function ManualFloat(TheScreenRect: TRect): Boolean;
    function ReplaceDockedControl(Control: TControl; NewDockSite: TWinControl;
      DropControl: TControl; ControlSide: TAlign): Boolean;
    Function  Dragging: Boolean;
  public
    // size
    procedure AdjustSize; virtual;
    function AutoSizeDelayed: boolean; virtual;
    procedure AnchorToNeighbour(Side: TAnchorKind; Space: integer;
                                Sibling: TControl);
    procedure AnchorParallel(Side: TAnchorKind; Space: integer;
                             Sibling: TControl);
    procedure AnchorHorizontalCenterTo(Sibling: TControl);
    procedure AnchorVerticalCenterTo(Sibling: TControl);
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer;
                                Lock: boolean); virtual;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
                               Raw: boolean); virtual;
    procedure InvalidatePreferredSize; virtual;
  public
    constructor Create(TheOwner: TComponent);override;
    destructor Destroy; override;
    procedure EditingDone; virtual;
    procedure ExecuteDefaultAction; virtual;
    procedure ExecuteCancelAction; virtual;
    procedure BeginDrag(Immediate: Boolean; Threshold: Integer);
    procedure BeginDrag(Immediate: Boolean);
    procedure BringToFront;
    function ColorIsStored: boolean; virtual;
    function HasParent: Boolean; override;
    function IsParentOf(AControl: TControl): boolean; virtual;
    function IsVisible: Boolean; virtual;
    procedure Hide;
    procedure Refresh;
    procedure Repaint; virtual;
    Procedure Invalidate; virtual;
    procedure AddControl; virtual;
    function CheckChildClassAllowed(ChildClass: TClass;
                                    ExceptionOnInvalid: boolean): boolean;
    procedure CheckNewParent(AParent: TWinControl); virtual;
    procedure SendToBack;
    procedure SetTempCursor(Value: TCursor);
    procedure UpdateRolesForForm; virtual;
    procedure ActiveDefaultControlChanged(NewControl: TControl); virtual;
    function  GetTextBuf(Buffer: PChar; BufSize: Integer): Integer; virtual;
    function  GetTextLen: Integer; virtual;
    Procedure SetTextBuf(Buffer: PChar); virtual;
    Function  Perform(Msg:Cardinal; WParam: WParam; LParam: LParam): LRESULT;
    Function  ScreenToClient(const APoint: TPoint): TPoint;
    Function  ClientToScreen(const APoint: TPoint): TPoint;
    Function  ScreenToControl(const APoint: TPoint): TPoint;
    Function  ControlToScreen(const APoint: TPoint): TPoint;
    procedure Show;
    procedure Update; virtual;
    procedure SetZOrderPosition(NewPosition: Integer); virtual;
    Procedure SetZOrder(TopMost: Boolean); virtual;
    function HandleObjectShouldBeVisible: boolean; virtual;
    function ParentDestroyingHandle: boolean;
    function ParentHandlesAllocated: boolean; virtual;
    procedure InitiateAction; virtual;
  public
    // Event lists
    procedure RemoveAllHandlersOfObject(AnObject: TObject); override;
    procedure AddHandlerOnResize(OnResizeEvent: TNotifyEvent; AsLast: boolean);
    procedure RemoveHandlerOnResize(OnResizeEvent: TNotifyEvent);
    procedure AddHandlerOnChangeBounds(OnChangeBoundsEvent: TNotifyEvent;
                                       AsLast: boolean);
    procedure RemoveHandlerOnChangeBounds(OnChangeBoundsEvent: TNotifyEvent);
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
    property ClientHeight: Integer read GetClientHeight write SetClientHeight stored False;
    property ClientOrigin: TPoint read GetClientOrigin;
    property ClientRect: TRect read GetClientRect;
    property ClientWidth: Integer read GetClientWidth write SetClientWidth stored False;
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
    property UndockHeight: Integer read GetUndockHeight write FUndockHeight;
    property UndockWidth: Integer read GetUndockWidth write FUndockWidth;
  published
    property AnchorSideLeft: TAnchorSide index 0 read GetAnchorSideIndex write SetAnchorSideIndex;
    property AnchorSideTop: TAnchorSide index 1 read GetAnchorSideIndex write SetAnchorSideIndex;
    property AnchorSideRight: TAnchorSide index 2 read GetAnchorSideIndex write SetAnchorSideIndex;
    property AnchorSideBottom: TAnchorSide index 3 read GetAnchorSideIndex write SetAnchorSideIndex;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
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

  TChildControlEnlargeStyle = (
      cesAnchorAligning, // (like Delphi)
      cesScaleChilds, // scale childs, keep space between childs fixed
      cesHomogenousChildGrowth, // enlarge childs equally
      cesHomogenousSpaceGrowth  // enlarge space between childs equally
    );
  TChildControlShrinkStyle = (
      cssAnchorAligning, // (like Delphi)
      cssScaleChilds, // scale childs
      cssHomogenousChildDecrease // shrink childs equally
    );

  TControlChildSizing = class(TPersistent)
  private
    FControl: TControl;
    FEnlargeHorizontal: TChildControlEnlargeStyle;
    FEnlargeVertical: TChildControlEnlargeStyle;
    FHorizontalSpacing: integer;
    FLeftRightSpacing: integer;
    FOnChange: TNotifyEvent;
    FShrinkHorizontal: TChildControlShrinkStyle;
    FShrinkVertical: TChildControlShrinkStyle;
    FTopBottomSpacing: integer;
    FVerticalSpacing: integer;
    procedure SetEnlargeHorizontal(const AValue: TChildControlEnlargeStyle);
    procedure SetEnlargeVertical(const AValue: TChildControlEnlargeStyle);
    procedure SetHorizontalSpacing(const AValue: integer);
    procedure SetLeftRightSpacing(const AValue: integer);
    procedure SetShrinkHorizontal(const AValue: TChildControlShrinkStyle);
    procedure SetShrinkVertical(const AValue: TChildControlShrinkStyle);
    procedure SetTopBottomSpacing(const AValue: integer);
    procedure SetVerticalSpacing(const AValue: integer);
  protected
    procedure Change; dynamic;
  public
    constructor Create(OwnerControl: TControl);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function IsEqual(Sizing: TControlChildSizing): boolean;
  public
    property Control: TControl read FControl;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    // TODO: publish the properties when implemented
    property EnlargeHorizontal: TChildControlEnlargeStyle read FEnlargeHorizontal
                           write SetEnlargeHorizontal default cesAnchorAligning;
    property EnlargeVertical: TChildControlEnlargeStyle read FEnlargeVertical
                             write SetEnlargeVertical default cesAnchorAligning;
    property ShrinkHorizontal: TChildControlShrinkStyle read FShrinkHorizontal
                            write SetShrinkHorizontal default cssAnchorAligning;
    property ShrinkVertical: TChildControlShrinkStyle read FShrinkVertical
                              write SetShrinkVertical default cssAnchorAligning;
  published
    property LeftRightSpacing: integer read FLeftRightSpacing write SetLeftRightSpacing;
    property TopBottomSpacing: integer read FTopBottomSpacing write SetTopBottomSpacing;
    property HorizontalSpacing: integer read FHorizontalSpacing write SetHorizontalSpacing;
    property VerticalSpacing: integer read FVerticalSpacing write SetVerticalSpacing;
  end;


  { TWinControlActionLink }

  TWinControlActionLink = class(TControlActionLink)
  protected
    procedure AssignClient(AClient: TObject); override;
    function IsHelpContextLinked: Boolean; override;
    procedure SetHelpContext(Value: THelpContext); override;
  end;

  TWinControlActionLinkClass = class of TWinControlActionLink;


  { TWinControl }
  
  TWinControlFlag = (
    wcfClientRectNeedsUpdate,
    wcfColorChanged,
    wcfFontChanged,
    wcfReAlignNeeded,
    wcfAligningControls,
    wcfEraseBackground,
    wcfAutoSizeNeeded,
    wcfCreatingHandle,      // Set while constructing the handle of this control
    wcfInitializing,        // Set while initializing during handle creation
    wcfCreatingChildHandles // Set while constructing the handles of the childs
    );
  TWinControlFlags = set of TWinControlFlag;

  { TWinControl }

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
    FControls: TList; // the child controls (only TControl, no TWinControl)
    FDefWndProc: Pointer;
    FDockClients: TList;
    //FDockSite: Boolean;
    FDoubleBuffered: Boolean;
    FClientWidth: Integer;
    FClientHeight: Integer;
    FDockManager: TDockManager;
    FDockSite: Boolean;
    FWinControlFlags: TWinControlFlags;
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
    FTabList: TList;
    FUseDockManager: Boolean;
    FWinControls: TList; // the child controls (only TWinControl, no TControl)
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
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure AdjustClientRect(var ARect: TRect); virtual;
    procedure AlignControls(AControl: TControl;
                            var RemainingClientRect: TRect); virtual;
    function DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                        AControlList: TList; var ARect: TRect): Boolean; virtual;
    procedure DoChildSizingChange(Sender: TObject); virtual;
    Function CanTab: Boolean; override;
    procedure DoDragMsg(var DragMsg: TCMDrag); override;
    Procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    procedure CMShowingChanged(var Message: TLMessage); message CM_SHOWINGCHANGED;
    procedure CMVisibleChanged(var TheMessage: TLMessage); message CM_VISIBLECHANGED;
    function  ContainsControl(Control: TControl): Boolean;
    procedure ControlsAligned; virtual;
    procedure DoSendBoundsToInterface; virtual;
    procedure RealizeBounds; virtual;
    procedure CreateSubClass(var Params: TCreateParams;ControlClassName: PChar);
    procedure DestroyComponent; virtual;
    procedure DoConstraintsChange(Sender: TObject); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure DoAutoSize; Override;
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer); override;
    procedure GetChildBounds(var ChildBounds: TRect; WithBorderSpace: boolean); virtual;
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
    procedure WMSysChar(var Message: TLMChar); message LM_SYSCHAR;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure WMDestroy(var Message: TLMDestroy); message LM_DESTROY;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure CNKeyDown(var Message: TLMKeyDown); message CN_KEYDOWN;
    procedure CNSysKeyDown(var Message: TLMKeyDown); message CN_SYSKEYDOWN;
    procedure CNKeyUp(var Message: TLMKeyUp); message CN_KEYUP;
    procedure CNSysKeyUp(var Message: TLMKeyUp); message CN_SYSKEYUP;
    procedure CNChar(var Message: TLMKeyUp); message CN_CHAR;
    procedure CNSysChar(var Message: TLMKeyUp); message CN_SYSCHAR;
  protected
    // drag and drop
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); dynamic;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
                       State: TDragState; var Accept: Boolean); dynamic;
    procedure DoDockOver(Source: TDragDockObject; X, Y: Integer;
                         State: TDragState; var Accept: Boolean); dynamic;
    procedure DoRemoveDockClient(Client: TControl); dynamic;
    function  DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; dynamic;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
                          MousePos: TPoint; var CanDock: Boolean); dynamic;
    procedure ReloadDockedControl(const AControlName: string;
                                  var AControl: TControl); dynamic;
    function CreateDockManager: TDockManager; dynamic;
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
    function  DoRemainingKeyPress(var Message: TLMKey): Boolean;
    function  DoRemainingKeyUp(var Message: TLMKeyDown): Boolean;
    function  DoKeyPress(var Message: TLMKey): Boolean;
    function  DoUTF8KeyPress(var UTF8Key: TUTF8Char): boolean; dynamic;
    function  DoKeyUpBeforeInterface(var Message: TLMKey): Boolean;
    function  ChildKey(var Message: TLMKey): boolean; dynamic;
    function  DialogChar(var Message: TLMKey): boolean; override;
    procedure ControlKeyDown(var Key: Word; Shift: TShiftState); dynamic;
    procedure ControlKeyUp(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyDownBeforeInterface(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyDownAfterInterface(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyPress(var Key: char); dynamic;
    procedure KeyUp(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyUpBeforeInterface(var Key: Word; Shift: TShiftState); dynamic;
    procedure KeyUpAfterInterface(var Key: Word; Shift: TShiftState); dynamic;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); dynamic;
  protected
    Function  FindNextControl(CurrentControl: TWinControl; GoForward,
                              CheckTabStop, CheckParent: Boolean): TWinControl;
    function  RealGetText: TCaption; override;
    function  GetBorderStyle: TBorderStyle;
    function  GetChildsRect(Scrolled: boolean): TRect; override;
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
    procedure FixupTabList;
    procedure FontChanged(Sender: TObject); override;
    procedure InitializeWnd; virtual; // gets called after the Handle is created and before the child handles are created
    procedure Loaded; override;
    procedure MainWndProc(var Msg: TLMessage);
    procedure ParentFormHandleInitialized; override;
    procedure ChildHandlesCreated; virtual;// called after childs handles are created
    procedure ReAlign; // realign all childs
    procedure RealSetText(const AValue: TCaption); override;
    procedure RemoveFocus(Removing: Boolean);
    procedure SendMoveSizeMessages(SizeChanged, PosChanged: boolean); override;
    procedure SetBorderStyle(NewStyle: TBorderStyle); virtual;
    procedure SetColor(Value: TColor); override;
    procedure SetZOrder(Topmost: Boolean); override;
    procedure SetZOrderPosition(NewPosition: Integer); override;
    procedure ShowControl(AControl: TControl); virtual;
    procedure Update; override;
    procedure UpdateControlState;
    procedure UpdateShowing; virtual;
    procedure WndProc(var Message: TLMessage); override;
  protected
    // properties which are not supported by all descendents
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle default bsNone;
    property OnGetSiteInfo: TGetSiteInfoEvent read FOnGetSiteInfo write FOnGetSiteInfo;
    {$ifdef ver1_0}
    // repeated as workaround for fpc 1.0.x bug,
    // which can't access a protected property defined in another unit.
    property WidgetSetClass;
    {$endif}
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
    procedure AdjustSize; override;
    function AutoSizeDelayed: boolean; override;
    procedure BeginUpdateBounds;
    procedure EndUpdateBounds;
    procedure LockRealizeBounds;
    procedure UnlockRealizeBounds;
    Function ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean): TControl;
    Function ControlAtPos(const Pos: TPoint;
      AllowDisabled, AllowWinControls: Boolean): TControl;
    Function ControlAtPos(const Pos: TPoint;
      AllowDisabled, AllowWinControls, OnlyClientAreas: Boolean): TControl; virtual;
    procedure DoAdjustClientRectChange;
    procedure InvalidateClientRectCache(WithChildControls: boolean);
    function ClientRectNeedsInterfaceUpdate: boolean;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
  public
    constructor Create(TheOwner: TComponent);override;
    constructor CreateParented(ParentWindow: HWnd);
    class function CreateParentedControl(ParentWindow: HWnd): TWinControl;
    destructor Destroy; override;
    procedure DockDrop(DockObject: TDragDockObject; X, Y: Integer); dynamic;
    Function CanFocus: Boolean;
    function GetControlIndex(AControl: TControl): integer;
    procedure SetControlIndex(AControl: TControl; NewIndex: integer);
    Function Focused: Boolean; virtual;
    function PerformTab(ForwardTab: boolean): boolean; virtual;
    function ControlByName(const ControlName: string): TControl;
    procedure SelectNext(CurControl: TWinControl;
                         GoForward, CheckTabStop: Boolean);
    Procedure BroadCast(var ToAllMessage);
    procedure NotifyControls(Msg: Word);
    procedure DefaultHandler(var AMessage); override;
    Procedure DisableAlign;
    Procedure EnableAlign;
    function  GetTextLen: Integer; override;
    Procedure Invalidate; override;
    Procedure InsertControl(AControl: TControl);
    Procedure InsertControl(AControl: TControl; Index: integer);
    Procedure RemoveControl(AControl: TControl);
    Procedure Insert(AControl: TControl);
    Procedure Insert(AControl: TControl; Index: integer);
    Procedure Remove(AControl: TControl);
    procedure Repaint; override;
    Procedure SetFocus; virtual;
    Function FindChildControl(const ControlName: String): TControl;
    procedure FlipChildren(AllLevels: Boolean); dynamic;
    Procedure GetTabOrderList(List: TList);
    function HandleAllocated: Boolean;
    procedure HandleNeeded;
    function BrushCreated: Boolean;
    procedure EraseBackground(DC: HDC); virtual;
    function IntfUTF8KeyPress(var UTF8Key: TUTF8Char;
                              RepeatCount: integer): boolean; dynamic;
  end;


  { TGraphicControl }

  TGraphicControl = class(TControl)
  private
    FCanvas: TCanvas;
    FOnPaint: TNotifyEvent;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  protected
    procedure Paint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;


  { TCustomControl }

  TCustomControl = class(TWinControl)
  private
    FCanvas: TCanvas;
    FOnPaint: TNotifyEvent;
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DestroyComponent; override;
    procedure Paint; virtual;
  public
    property Canvas: TCanvas read FCanvas write FCanvas;
    property BorderStyle;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;


  { TImageList }

  TImageList = class(TDragImageList)
  published
    property BkColor: TColor;
    Property Height;
    property Masked;
    Property Width;
    Property OnChange;
  end;


  {$IFNDEF VER1_0}
  { TControlPropertyStorage }

  TControlPropertyStorage = class(TCustomPropertyStorage)
  protected
    procedure GetPropertyList(List: TStrings); override;
  end;
  {$ENDIF not VER1_0}


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
    FZoneLimit: integer;
    FParentZone: TDockZone;
    FOrientation: TDockOrientation;
    FNextSibling: TDockZone;
    FPrevSibling: TDockZone;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetLimitBegin: Integer;
    function GetLimitSize: Integer;
    function GetTop: Integer;
    function GetVisible: Boolean;
    function GetVisibleChildCount: Integer;
    function GetWidth: Integer;
    function GetZoneLimit: Integer;
    procedure SetZoneLimit(const AValue: Integer);
    function IsOrientationValid: boolean;
    function GetNextVisibleZone: TDockZone;
  public
    constructor Create(TheTree: TDockTree);
    procedure ExpandZoneLimit(NewLimit: Integer);
    function FirstVisibleChild: TDockZone;
    function NextVisible: TDockZone;
    function PrevVisible: TDockZone;
    procedure ResetChildren;
    procedure ResetZoneLimits;
    procedure Update;
    property Tree: TDockTree read FTree;
    property ChildCount: Integer read FChildCount;
    property Height: Integer read GetHeight;
    property Left: Integer read GetLeft;
    property LimitBegin: Integer read GetLimitBegin;
    property LimitSize: Integer read GetLimitSize;
    property Top: Integer read GetTop;
    property Visible: Boolean read GetVisible;
    property VisibleChildCount: Integer read GetVisibleChildCount;
    property Width: Integer read GetWidth;
    property ZoneLimit: Integer read GetZoneLimit write SetZoneLimit;
  end;
  TDockZoneClass = class of TDockZone;


  { TDockTree - a tree of TDockZones - Every docked window has one tree
  
    This is an abstract class. The real implementation is in ldocktree.pas

    Docking means here: Combining several windows to one. A window can here be
    a TCustomForm or a floating control (undocked) or a TDockForm.
    A window can be docked to another to the left, right, top, bottom or "into".
    The docking source window will be resized, to fit to the docking target
    window.

    Example1: Docking "A" left to "B"
    
       +---+    +----+
       | A | -> | B  |
       +---+    |    |
                +----+
      Result: A new docktree will be created. Height of "A" will be resized to
              the height of "B".
              A splitter will be inserted between "A" and "B".
              And all three are childs of the newly created TDockForm of the
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
      the docktree of "B".
      
      
    Example2: Docking A into B
                +-----+
       +---+    |     |
       | A | ---+-> B |
       +---+    |     |
                +-----+

      Result: A new docktree will be created. "A" will be resized to the size
              of "B". Both will be put into a TDockPages control which is the
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
  }

  TForEachZoneProc = procedure(Zone: TDockZone) of object;

  TDockTreeClass = class of TDockTree;

  TDockTreeFlag = (
    dtfUpdateAllNeeded
    );
  TDockTreeFlags = set of TDockTreeFlag;

  { TDockTree }

  TDockTree = class(TDockManager)
  private
    FBorderWidth: Integer;
    FDockSite: TWinControl;
    FDockZoneClass: TDockZoneClass;
    FGrabberSize: Integer;
    FGrabbersOnTop: Boolean;
    FFlags: TDockTreeFlags;
    FTopZone: TDockZone;
    FTopXYLimit: Integer;
    FUpdateCount: Integer;
    procedure DeleteZone(Zone: TDockZone);
  protected
    procedure AdjustDockRect(AControl: TControl; var ARect: TRect); virtual;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure GetControlBounds(AControl: TControl; var ControlBounds: TRect); override;
    function HitTest(const MousePos: TPoint; var HTFlag: Integer): TControl; virtual;
    procedure InsertControl(AControl: TControl; InsertAt: TAlign;
                            DropControl: TControl); override;
    procedure LoadFromStream(SrcStream: TStream); override;
    procedure PaintDockFrame(ACanvas: TCanvas; AControl: TControl;
                             const ARect: TRect); virtual;
    procedure PositionDockRect(AClient, DropCtl: TControl; DropAlign: TAlign;
                               var DockRect: TRect); override;
    procedure RemoveControl(AControl: TControl); override;
    procedure SaveToStream(DestStream: TStream); override;
    procedure SetReplacingControl(AControl: TControl); override;
    procedure ResetBounds(Force: Boolean); override;
    procedure UpdateAll;
    property DockSite: TWinControl read FDockSite write FDockSite;
  public
    constructor Create(TheDockSite: TWinControl); virtual;
    destructor Destroy; override;
    procedure PaintSite(DC: HDC); override;
    property DockZoneClass: TDockZoneClass read FDockZoneClass;
  end;


  { TMouse }

  TMouse = class
    FCapture: HWND;
    FDragImmediate: Boolean;
    FDragThreshold: Integer;
    Procedure SetCapture(const Value: HWND);
    Function GetCapture: HWND;
    Function GetCursorPos: TPoint;
    function GetIsDragging: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Capture: HWND read GetCapture write SetCapture;
    property CursorPos: TPoint read GetCursorPos;
    property DragImmediate: Boolean read FDragImmediate write FDragImmediate default True;
    property DragThreshold: Integer read FDragThreshold write FDragThreshold default 5;
    property IsDragging: Boolean read GetIsDragging;
  end;


const
  AnchorAlign: array[TAlign] of TAnchors = (
    { alNone }
    [akLeft, akTop],
    { alTop }
    [akLeft, akTop, akRight],
    { alBottom }
    [akLeft, akRight, akBottom],
    { alLeft }
    [akLeft, akTop, akBottom],
    { alRight }
    [akRight, akTop, akBottom],
    { alClient }
    [akLeft, akTop, akRight, akBottom],
    { alCustom }
    [akLeft, akTop]
    );
  DefaultSideForAnchorKind: array[TAnchorKind] of TAnchorSideReference = (
    // akTop
    asrBottom,
    // akLeft
    asrBottom,
    // akRight
    asrTop,
    // akBottom
    asrTop
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


function FindDragTarget(const Position: TPoint; AllowDisabled: Boolean): TControl;
Function FindControlAtPosition(const Position: TPoint; AllowDisabled: Boolean): TControl;
Function FindLCLWindow(const ScreenPos: TPoint): TWinControl;
Function FindControl(Handle: hwnd): TWinControl;
Function FindOwnerControl(Handle: hwnd): TWinControl;
function FindLCLControl(const ScreenPos: TPoint): TControl;

function SendAppMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): Longint;
procedure MoveWindowOrg(dc: hdc; X,Y: Integer);

// Interface support.

procedure RecreateWnd(const AWinControl:TWinControl);


// drag and drop

var
  DefaultDockTreeClass: TDockTreeClass;

procedure SetCaptureControl(Control: TControl);
function GetCaptureControl: TControl;
procedure CancelDrag;
procedure DragDone(Drop: Boolean);

var
  NewStyleControls: Boolean;
  Mouse: TMouse;

// cursor
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


// register (called by the package initialization in design mode)
procedure Register;


implementation

uses
  WSControls, // Widgetset uses circle is allowed
  Forms, // the circle can't be broken without breaking Delphi compatibility
  Math;  // Math is in RTL and only a few functions are used.

var
  // The interface knows, which TWinControl has the capture. This stores
  // what child control of this TWinControl has actually the capture.
  CaptureControl: TControl;
  DockSiteHash: TDynHashArray;

procedure AdjustBorderSpace(var RemainingClientRect, CurBorderSpace: TRect;
  Left, Top, Right, Bottom: integer);
// RemainingClientRect: remaining clientrect without CurBorderSpace
// CurBorderSpace: current borderspace around RemainingClientRect
// Left, Top, Right, Bottom: apply these borderspaces to CurBorderSpace
//
// CurBorderSpace will be set to the maximum of CurBorderSpace and Left, Top,
// Right, Bottom. RemainingClientRect will shrink.
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

{------------------------------------------------------------------------------}
{ RecreateWnd                                                                  }
{ This function was originally member of TWincontrol. From a VCL point of view }
{ that made perfectly sense since the VCL knows when a win32 widget has to be  }
{ recreated when properties have changed.                                      }
{ The LCL however doesn't know, the widgetset does. To avoid old VCL behaviour }
{ and to provide a central function to the widgetset, it is moved here.        }
{ MWE.                                                                         }
{------------------------------------------------------------------------------}
procedure RecreateWnd(const AWinControl:TWinControl);
var
  IsFocused: Boolean;
begin
  if csDestroying in AWinControl.ComponentState then Exit;

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
  IMPORTANT: Hence, in most cases: parameter Handle <> Result.Handle
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
  Result:=AWinControl.ControlAtPos(ClientPos,true,true,false);
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
  Result := False;
  AWinControl := FindOwnerControl(Handle);
  if AWinControl <> nil then begin
    with TLMessage(Message) do
      AWinControl.Perform(Msg + CN_BASE, WParam, LParam);
    Result:= True;
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
                                       AllowDisabled,true);
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

procedure SetCaptureControl(Control: TControl);
var
  OldCaptureWinControl: TWinControl;
  NewCaptureWinControl: TWinControl;
begin
  if CaptureControl=Control then exit;
  if Control=nil then begin
    {$IFDEF VerboseMouseCapture}
    write('SetCaptureControl Only ReleaseCapture');
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
    write('SetCaptureControl Only ReleaseCapture');
    {$ENDIF}
    // just unset the capturing, intf call not needed
    CaptureControl:=nil;
    ReleaseCapture;
    exit;
  end;
  if NewCaptureWinControl=OldCaptureWinControl then begin
    {$IFDEF VerboseMouseCapture}
    write('SetCaptureControl Keep WinControl ',NewCaptureWinControl.Name,':',NewCaptureWinControl.ClassName,
    ' switch Control ',Control.Name,':',Control.ClassName);
    {$ENDIF}
    // just change the CaptureControl, intf call not needed
    CaptureControl:=Control;
    exit;
  end;
  // switch capture control
  {$IFDEF VerboseMouseCapture}
    write('SetCaptureControl Switch to WinControl=',NewCaptureWinControl.Name,':',NewCaptureWinControl.ClassName,
    ' and Control=',Control.Name,':',Control.ClassName);
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
{$I basedragcontrolobject.inc}
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

{ TControlBorderSpacing }

procedure TControlBorderSpacing.SetAround(const AValue: TSpacingSize);
begin
  if FAround=AValue then exit;
  FAround:=AValue;
  Change;
end;

procedure TControlBorderSpacing.SetBottom(const AValue: TSpacingSize);
begin
  if FBottom=AValue then exit;
  FBottom:=AValue;
  Change;
end;

procedure TControlBorderSpacing.SetLeft(const AValue: TSpacingSize);
begin
  if FLeft=AValue then exit;
  FLeft:=AValue;
  Change;
end;

procedure TControlBorderSpacing.SetRight(const AValue: TSpacingSize);
begin
  if FRight=AValue then exit;
  FRight:=AValue;
  Change;
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
  Change;
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
    
    Change;
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
  Result:=Around;
  case Kind of
  akLeft: inc(Result,Left);
  akTop: inc(Result,Top);
  akRight: inc(Result,Right);
  akBottom: inc(Result,Bottom);
  end;
end;

procedure TControlBorderSpacing.Change;
begin
  FControl.DoBorderSpacingChange(Self);
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TControlChildSizing }

procedure TControlChildSizing.SetEnlargeHorizontal(
  const AValue: TChildControlEnlargeStyle);
begin
  if FEnlargeHorizontal=AValue then exit;
  FEnlargeHorizontal:=AValue;
  Change;
end;

procedure TControlChildSizing.SetEnlargeVertical(
  const AValue: TChildControlEnlargeStyle);
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

procedure TControlChildSizing.SetLeftRightSpacing(const AValue: integer);
begin
  if FLeftRightSpacing=AValue then exit;
  FLeftRightSpacing:=AValue;
  Change;
end;

procedure TControlChildSizing.SetShrinkHorizontal(
  const AValue: TChildControlShrinkStyle);
begin
  if FShrinkHorizontal=AValue then exit;
  FShrinkHorizontal:=AValue;
  Change;
end;

procedure TControlChildSizing.SetShrinkVertical(
  const AValue: TChildControlShrinkStyle);
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

constructor TControlChildSizing.Create(OwnerControl: TControl);
begin
  FControl:=OwnerControl;
  inherited Create;
  FEnlargeHorizontal:=cesAnchorAligning;
  FEnlargeVertical:=cesAnchorAligning;
  FShrinkHorizontal:=cssAnchorAligning;
  FShrinkVertical:=cssAnchorAligning;
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
      and (FShrinkVertical=Sizing.ShrinkVertical);
end;

procedure TControlChildSizing.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TAnchorSide }

procedure TAnchorSide.SetControl(const AValue: TControl);

  procedure RaiseOwnerCircle;
  begin
    raise Exception.Create('TAnchorSide.SetControl AValue=FOwner');
  end;

begin
  if (AValue=FOwner) then RaiseOwnerCircle;
  if FControl=AValue then exit;
  FControl:=AValue;
  //debugln('TAnchorSide.SetControl A ',DbgSName(FOwner));
  FOwner.AnchorSideChanged(Self);
end;

function TAnchorSide.IsSideStored: boolean;
begin
  Result:=(Control<>nil) and (FSide<>DefaultSideForAnchorKind[FKind]);
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
end;

constructor TAnchorSide.Create(TheOwner: TControl; TheKind: TAnchorKind);
begin
  inherited Create;
  FOwner:=TheOwner;
  FKind:=TheKind;
  FSide:=DefaultSideForAnchorKind[FKind];
end;

procedure TAnchorSide.GetSidePosition(var ReferenceControl: TControl;
  var ReferenceSide: TAnchorSideReference; var Position: Integer);
  
  procedure RaiseInvalidSide;
  begin
    raise Exception.Create('TAnchorSide.GetSidePosition invalid Side');
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
      ReferenceControl:=nil;
      exit;
    end;
    
    // check if ReferenceControl is valid
    if (ReferenceControl.Parent<>OwnerParent)
    and (ReferenceControl<>OwnerParent) then begin
      // not a sibling and not the parent -> invalid AnchorSide
      ReferenceControl:=nil;
      exit;
    end;
    
    if ReferenceControl.Visible
    or ([csDesigning,csNoDesignVisible]*ReferenceControl.ComponentState
        =[csDesigning])
    then begin
      // ReferenceControl is visible
      // -> calculate Position
      OwnerBorderSpacing:=FOwner.BorderSpacing.GetSpace(Kind);
      case ReferenceSide of
      
      asrTop:
        if Kind in [akLeft,akRight] then begin
          // anchor to left side of ReferenceControl
          if ReferenceControl=OwnerParent then
            Position:=0
          else
            Position:=ReferenceControl.Left;
          if Kind=akLeft then begin
            // anchor left of ReferenceControl and left of Owner
            inc(Position,OwnerBorderSpacing);
          end else begin
            // anchor left of ReferenceControl and right of Owner
            if ReferenceControl=OwnerParent then
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                      OwnerParent.ChildSizing.LeftRightSpacing)
            else
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                               ReferenceControl.BorderSpacing.GetSpace(akLeft));
            dec(Position,OwnerBorderSpacing);
          end;
        end else begin
          // anchor to top side of ReferenceControl
          if ReferenceControl=OwnerParent then
            Position:=0
          else
            Position:=ReferenceControl.Top;
          if Kind=akTop then begin
            // anchor top of ReferenceControl and top of Owner
            inc(Position,OwnerBorderSpacing);
          end else begin
            // anchor top of ReferenceControl and bottom of Owner
            if ReferenceControl=OwnerParent then
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                      OwnerParent.ChildSizing.TopBottomSpacing)
            else
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                ReferenceControl.BorderSpacing.GetSpace(akTop));
            dec(Position,OwnerBorderSpacing);
          end;
        end;

      asrBottom:
        if Kind in [akLeft,akRight] then begin
          // anchor to right side of ReferenceControl
          if ReferenceControl=OwnerParent then
            Position:=OwnerParent.ClientWidth
          else
            Position:=ReferenceControl.Left+ReferenceControl.Width;
          if Kind=akLeft then begin
            // anchor right of ReferenceControl and left of Owner
            if ReferenceControl=OwnerParent then
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                      OwnerParent.ChildSizing.LeftRightSpacing)
            else
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                              ReferenceControl.BorderSpacing.GetSpace(akRight));
            inc(Position,OwnerBorderSpacing);
          end else begin
            // anchor right of ReferenceControl and right of Owner
            dec(Position,OwnerBorderSpacing);
          end;
        end else begin
          // anchor to bottom side of ReferenceControl
          if ReferenceControl=OwnerParent then
            Position:=OwnerParent.ClientHeight
          else
            Position:=ReferenceControl.Top+ReferenceControl.Height;
          if Kind=akTop then begin
            // anchor bottom of ReferenceControl and top of Owner
            if ReferenceControl=OwnerParent then
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                      OwnerParent.ChildSizing.TopBottomSpacing)
            else
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                             ReferenceControl.BorderSpacing.GetSpace(akBottom));
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
    if ReferenceControl=OwnerParent then
      NextReferenceSide:=nil
    else
      NextReferenceSide:=ReferenceControl.AnchorSide[
                                       AnchorReferenceSide[Kind,ReferenceSide]];
    if (NextReferenceSide=nil) then begin
      ReferenceControl:=nil;
      exit;
    end;
    ReferenceControl:=NextReferenceSide.Control;
    ReferenceSide:=NextReferenceSide.Side;
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

{$IFNDEF VER1_0}
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
{$ENDIF not VER1_0}

initialization

  //DebugLn('controls.pp - initialization');
  Mouse := TMouse.Create;
  DragControl := nil;
  CaptureControl := nil;
  DefaultDockTreeClass := TDockTree;
  DockSiteHash := nil;

  RegisterIntegerConsts(TypeInfo(TCursor), @IdentToCursor, @CursorToIdent);

finalization
  FreeThenNil(DockSiteHash);
  FreeThenNil(Mouse);

end.

{ =============================================================================

  $Log$
  Revision 1.298  2005/06/02 12:11:54  micha
  need to recreate form window when changing border flags
  implement lock-flag when destroying handle, do not focus control that is destroying handles

  Revision 1.297  2005/04/22 09:29:30  mattias
  made TContro.AutoSize public

  Revision 1.296  2005/04/20 07:14:00  mattias
  moved THintInfo from forms.pp to controls.pp and changed TControlShowHint event from pointer to PHintHinfo

  Revision 1.295  2005/04/17 18:41:15  micha
  implement active default control switching
  pressing return key executes active default control action

  Revision 1.294  2005/04/04 11:43:36  mattias
  fixed compilation and designtime drawing of gtkglarea

  Revision 1.293  2005/04/01 19:10:42  micha
  make method Hide a member of TControl (fixes bug 707)

  Revision 1.292  2005/03/31 20:49:39  micha
  remove TMouse.SetCursorPos (tricky and not cross-platform)

  Revision 1.291  2005/03/30 18:34:42  micha
  fix bug 659 and 660: LM_RBUTTONUP message returns true whenever popupmenu was invoked

  Revision 1.290  2005/03/25 16:41:00  micha
  make DestroyHandle method protected again, use global RecreateWnd procedure

  Revision 1.289  2005/03/25 08:58:11  micha
  implement ShowInTaskBar for win32 intf

  Revision 1.288  2005/03/11 14:40:37  mattias
  moved CM_ message constants from crontrols.pp to lmessages.pp to break circles and clean up controls.pp

  Revision 1.287  2005/03/05 19:45:22  mattias
  made New Dialog buttons context sensitive

  Revision 1.286  2005/03/04 17:55:34  micha
  fix bug 605: resizing upward or leftward should not move control

  Revision 1.285  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.284  2005/02/21 20:15:27  mattias
  fixed componentpalette adding via double click

  Revision 1.283  2005/02/19 21:54:08  mattias
  moved LCL navigation key handling to key up, so that interface has the chance to handle keys

  Revision 1.282  2005/02/16 22:55:59  mattias
  improved gtk intf file dialog filter  from C Western

  Revision 1.281  2005/02/08 21:46:22  vincents
  fixed fpc 1.0.x compilation

  Revision 1.280  2005/02/05 16:09:52  marc
  * first 64bit changes

  Revision 1.279  2005/02/03 15:10:22  micha
  implement shortcut handling, tcustomlabel accelerator focuscontrol functionality

  Revision 1.278  2005/01/26 23:23:11  mattias
  added error when setting FormStyle to MDI

  Revision 1.277  2005/01/26 15:45:08  mattias
  implemented adding files from directory in project inspector, fixed extrac proc checking overlapping blocks

  Revision 1.276  2005/01/25 09:58:16  mattias
  fixed fpc 1.0.10 compilation

  Revision 1.275  2005/01/24 14:57:36  mattias
  fixed TCollectionPropertyEditorForm to recognize renames and deletes

  Revision 1.274  2005/01/24 12:23:11  mattias
  fixed TColorButton.Paint

  Revision 1.273  2005/01/22 22:26:16  mattias
  added sprite example

  Revision 1.272  2005/01/21 11:52:01  micha
  cleanup focus; fix tabbing

  Revision 1.271  2005/01/21 10:34:56  mattias
  implemented streaming of anchorsides

  Revision 1.270  2005/01/20 20:51:06  mattias
  implementing anchor editor, setting sibling and reference sides

  Revision 1.269  2005/01/17 17:49:27  mattias
  fixed constraints for forms under gtk

  Revision 1.268  2005/01/17 16:42:35  mattias
  improved TLabel autosizing

  Revision 1.267  2005/01/17 11:53:39  mattias
  added showing all four sides to AnchorEditor

  Revision 1.266  2005/01/13 22:07:10  mattias
  added mouse cursors for 8 uni directions, imlemented for gtk

  Revision 1.265  2005/01/08 14:23:56  micha
  move taborder and tabstop to twincontrol

  Revision 1.264  2005/01/07 21:02:48  micha
  add tcontrolclass, needed by codetoolsdefines

  Revision 1.263  2005/01/03 22:44:31  mattias
  implemented TControl.AnchorSide

  Revision 1.262  2004/12/27 19:40:59  mattias
  published BorderSpacing for many controls

  Revision 1.261  2004/12/27 12:56:42  mattias
  started TTranslateStrings and .lrt files support  from Vasily

  Revision 1.260  2004/12/23 00:33:43  mattias
  fixed crash on readonly projects

  Revision 1.259  2004/12/22 23:54:20  mattias
  started TControl.AnchorSide

  Revision 1.258  2004/12/20 00:11:24  mattias
  changed TControl.Anchors default value to AnchorAlign[Align]

  Revision 1.257  2004/11/29 01:12:36  mattias
  added SysKey messages to gtk intf and LCL

  Revision 1.256  2004/11/05 22:08:53  mattias
  implemented auto sizing: child to parent sizing

  Revision 1.255  2004/11/03 14:18:35  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.254  2004/10/30 16:24:06  mattias
  disabled alClient RemainingClientRect adjust

  Revision 1.253  2004/10/29 06:57:22  vincents
  fixed fpc 1.0.x compilation

  Revision 1.252  2004/10/28 17:56:10  mattias
  implemented Borderspacing

  Revision 1.251  2004/10/28 09:30:49  mattias
  implemented borderspacing TWinControl.ChildSizing.Left/Top

  Revision 1.250  2004/10/12 08:23:20  mattias
  fixed compiler options interface double variables

  Revision 1.249  2004/09/25 15:05:38  mattias
  implemented Rename Identifier

  Revision 1.248  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.247  2004/09/14 10:23:44  mattias
  implemented finding DefineProperties in registered TPersistent, implemented auto commenting of missing units for Delphi unit conversion

  Revision 1.246  2004/09/09 22:00:37  mattias
  started TTabControlNotebookStrings

  Revision 1.245  2004/09/08 22:59:54  mattias
  started TTabControl

  Revision 1.244  2004/09/08 08:20:50  mattias
  fixed find in files searching at end of line

  Revision 1.243  2004/09/02 09:16:58  mattias
  improved double byte char fonts for gtk1, started synedit UTF8 support

  Revision 1.242  2004/08/30 16:37:58  mattias
  added OnUTF8KeyPresss

  Revision 1.241  2004/08/30 16:11:02  mattias
  changed GTK2 IFDEF to USE_UTF8BIDI_LCL

  Revision 1.240  2004/08/26 19:09:33  mattias
  moved navigation key handling to TApplication and added options for custom navigation

  Revision 1.239  2004/08/21 23:16:11  mattias
  implemented simple HTML help viewer

  Revision 1.238  2004/08/18 22:56:11  mattias
  implemented basic manual docking

  Revision 1.237  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.236  2004/08/18 09:08:33  mattias
  fixed deleting of collection item in prop editor

  Revision 1.235  2004/08/16 20:40:26  mattias
  published TForm.SessionProperties, added property editor and activated the storage components for fpc 1.9.5 because of rttiutils

  Revision 1.234  2004/08/13 16:40:47  mazen
  + TCharater type used to allow UTF8 keyboard with gtk2

  Revision 1.233  2004/08/09 21:12:42  mattias
  implemented FormStyle fsSplash for splash screens

  Revision 1.232  2004/08/05 21:20:47  mattias
  moved designer/abstractformeditor.pp to ideintf/formeditingintf.pas

  Revision 1.231  2004/08/03 09:01:54  mattias
  LCL now handles for non win32 CN_CHAR

  Revision 1.230  2004/07/25 22:54:38  mattias
  fixed fpc 1.0.10 compilation

  Revision 1.229  2004/07/25 15:39:55  mattias
  added rx components  from Michal Van Canneyt

  Revision 1.228  2004/07/25 01:04:45  mattias
  TXMLPropStorage basically working

  Revision 1.227  2004/07/23 22:14:22  mattias
  fixed compilation for fpc 1.0.10

  Revision 1.226  2004/07/14 15:57:53  mattias
  fixed 1.0.10 compilation  from Vincent

  Revision 1.225  2004/07/11 13:03:53  mattias
  extended RolesForForm to manage multiple roles for on control

  Revision 1.224  2004/07/07 22:26:57  mattias
  fixed showing grabers for boundless components

  Revision 1.223  2004/07/04 20:07:08  micha
  form notifies control of new role

  Revision 1.222  2004/07/03 14:59:42  mattias
  fixed keydown geting all keys

  Revision 1.221  2004/07/03 13:06:29  mattias
  improved key handling for OI

  Revision 1.220  2004/07/03 11:11:08  mattias
  TGTKListStringList now keeps selection on Put and Move

  Revision 1.219  2004/07/01 20:42:11  micha
  implement better ExecuteXXAction design; break dependency on TButton class in TCustomForm

  Revision 1.218  2004/07/01 10:08:31  mattias
  made key handling more flexible

  Revision 1.217  2004/06/30 11:07:20  micha
  implement return key clicks default button; escape key clicks cancel button

  Revision 1.216  2004/06/29 10:23:00  micha
  fix cnkeydown to check wm_getdlgcode result
  fix win32 intf to also send wm_keydown of cn_keydown wasn't processed

  Revision 1.215  2004/06/28 23:46:40  marc
  * Fixed compilation on 1.0.10
  * Fixed check for override of GetTextBuf and SetTextBuf

  Revision 1.214  2004/06/28 18:47:30  mattias
  further fixed GetControlAtPos

  Revision 1.213  2004/06/28 08:54:19  mattias
  fixed ord ptr conversion hints

  Revision 1.212  2004/06/24 17:59:18  mattias
  fixed compilation for fpc 1.0.10

  Revision 1.211  2004/06/20 21:21:49  micha
  fix GetVisible to return this control's visibility, instead introduce IsVisible to check for recursive visibility

  Revision 1.210  2004/06/20 20:25:47  micha
  fix tabbing to next control to skip invisible notebook pages

  Revision 1.209  2004/06/14 12:54:02  micha
  fix designer cursor to not set Form.Cursor directly

  Revision 1.208  2004/06/01 22:49:50  mattias
  added workaround for buggy typinfo GetMethodProp function

  Revision 1.207  2004/06/01 09:58:35  mattias
  implemented setting TCustomPage.PageIndex  from Andrew Haines

  Revision 1.206  2004/05/30 20:17:55  vincents
  changed radiobutton style to BS_RADIOBUTTON to prevent test program from hanging.

  Revision 1.205  2004/05/30 14:02:30  mattias
  implemented OnChange for TRadioButton, TCheckBox, TToggleBox and some more docking stuff

  Revision 1.204  2004/05/22 14:35:32  mattias
  fixed button return key

  Revision 1.203  2004/05/21 18:34:44  mattias
  readded protected TWinControl.BorderStyle

  Revision 1.202  2004/05/21 18:12:17  mattias
  quick fixed crashing property overloading BorderStyle

  Revision 1.201  2004/05/21 09:03:54  micha
  implement new borderstyle
  - centralize to twincontrol (protected)
  - public expose at tcustomcontrol to let interface access it

  Revision 1.200  2004/05/11 12:16:47  mattias
  replaced writeln by debugln

  Revision 1.199  2004/05/11 09:49:46  mattias
  started sending CN_KEYUP

  Revision 1.198  2004/04/26 10:01:27  mattias
  fixed TSynEdit.RealGetText

  Revision 1.197  2004/04/20 23:39:01  marc
  * Fixed setting of TWincontrol.Text during load

  Revision 1.196  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.195  2004/04/11 10:19:28  micha
  cursor management updated:
  - lcl notifies interface via WSControl.SetCursor of changes
  - fix win32 interface to respond to wm_setcursor callback and set correct cursor

  Revision 1.194  2004/04/09 23:52:01  mattias
  fixed hiding uninitialized controls

  Revision 1.193  2004/04/04 12:32:21  mattias
  TWinControl.CanTab now checks for CanFocus

  Revision 1.192  2004/03/25 14:07:24  vincents
  use only key down (not toggle) state in GetKeyState

  Revision 1.191  2004/03/19 00:03:14  marc
  * Moved the implementation of (GTK)ButtonCreateHandle to the new
    (GTK)WSButton class

  Revision 1.190  2004/03/17 00:34:37  marc
  * Interface reconstruction. Created skeleton units, classes and wscontrols

  Revision 1.189  2004/03/15 09:06:57  mattias
  added FindDragTarget

  Revision 1.188  2004/03/08 22:36:01  mattias
  added TWinControl.ParentFormInitializeWnd

  Revision 1.187  2004/03/07 09:37:20  mattias
  added workaround for AutoSize in TCustomLabel

  Revision 1.186  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.185  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.184  2004/02/24 21:53:12  mattias
  added StdActns definitions, no code yet

  Revision 1.183  2004/02/23 23:15:12  mattias
  improved FindDragTarget

  Revision 1.182  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.181  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.179  2004/02/22 15:39:43  mattias
  fixed error handling on saving lpi file

  Revision 1.178  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.177  2004/02/21 15:37:33  mattias
  moved compiler options to project menu, added -CX for smartlinking

  Revision 1.176  2004/02/17 00:32:25  mattias
  fixed TCustomImage.DoAutoSize fixing uninitialized vars

  Revision 1.175  2004/02/13 15:49:54  mattias
  started advanced LCL auto sizing

  Revision 1.174  2004/02/12 18:09:10  mattias
  removed win32 specific TToolBar code in new TToolBar, implemented TWinControl.FlipChildren

  Revision 1.173  2004/02/04 23:30:18  mattias
  completed TControl actions

  Revision 1.172  2004/02/02 16:59:28  mattias
  more Actions  TAction, TBasicAction, ...

  Revision 1.171  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.170  2004/02/02 11:07:43  mattias
  constraints and aligning now work together

  Revision 1.169  2004/02/02 00:41:06  mattias
  TScrollBar now automatically checks Align and Anchors for useful values

  Revision 1.168  2004/01/27 21:32:11  mattias
  improved changing style of controls

  Revision 1.167  2004/01/07 18:05:46  micha
  add TWinControl.DoubleBuffered property which is a hint for the interface to do double-buffering for this control

  Revision 1.166  2004/01/03 23:14:59  mattias
  default font can now change height and fixed gtk crash

  Revision 1.165  2004/01/03 21:06:05  micha
  - fix win32/checklistbox
  - implement proper lcl to interface move/size notify via setwindowpos
  - fix treeview to use inherited canvas from customcontrol
  - implement double buffering in win32

  Revision 1.164  2004/01/03 18:16:25  mattias
  set DragCursor props to default

  Revision 1.163  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.162  2003/12/27 20:15:15  mattias
  set some colors to default

  Revision 1.161  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.160  2003/12/23 16:50:45  micha
  fix defocus control when destroying it

  Revision 1.159  2003/12/14 19:18:03  micha
  hint fixes: parentfont, font itself, showing/hiding + more

  Revision 1.158  2003/11/22 17:22:14  mattias
  moved TBevelCut to controls.pp

  Revision 1.157  2003/11/03 16:57:47  peter
    * change $ifdef ver1_1 to $ifndef ver1_0 so it works also with
      fpc 1.9.x

  Revision 1.156  2003/10/16 19:43:44  ajgenius
  disable Buffering in TWinControl.WM_PAINT

  Revision 1.155  2003/10/06 10:50:10  mattias
  added recursion to InvalidateClientRectCache

  Revision 1.154  2003/09/26 06:59:59  mattias
  implemented GetBrush

  Revision 1.153  2003/09/23 17:52:04  mattias
  added SetAnchors

  Revision 1.152  2003/09/23 08:00:46  mattias
  improved OnEnter for gtkcombo

  Revision 1.151  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.150  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.149  2003/09/13 15:51:21  mattias
  implemented parent color from Micha

  Revision 1.148  2003/09/02 08:39:16  mattias
  added italian localization

  Revision 1.147  2003/08/27 20:55:51  mattias
  fixed updating codetools on changing pkg output dir

  Revision 1.146  2003/08/27 11:01:10  mattias
  started TDockTree

  Revision 1.145  2003/08/26 20:30:39  mattias
  fixed updating component tree on delete component

  Revision 1.144  2003/08/25 16:18:15  mattias
  fixed background color of TPanel and clicks of TSpeedButton from Micha

  Revision 1.143  2003/08/23 21:17:08  mattias
  several fixes for the win32 intf, added pending OnResize events

  Revision 1.142  2003/08/23 11:30:50  mattias
  fixed SetComboHeight in win32 intf and finddeclaration of overloaded proc definition

  Revision 1.141  2003/08/21 13:04:10  mattias
  implemented insert marks for TTreeView

  Revision 1.140  2003/08/14 15:31:42  mattias
  started TTabSheet and TPageControl

  Revision 1.139  2003/08/04 08:43:20  mattias
  fixed breaking circle in ChangeBounds

  Revision 1.138  2003/07/30 13:03:44  mattias
  replaced label with memo

  Revision 1.137  2003/07/24 06:54:32  mattias
  fixed anti circle mechnism for aligned controls

  Revision 1.136  2003/07/04 10:12:16  mattias
  added default message handler to win32 interface

  Revision 1.135  2003/06/30 14:58:29  mattias
  implemented multi file add to package editor

  Revision 1.134  2003/06/27 23:42:38  mattias
  fixed TScrollBar resizing

  Revision 1.133  2003/06/25 18:12:32  mattias
  added docking properties

  Revision 1.132  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.131  2002/08/19 15:15:23  mattias
  implemented TPairSplitter

  Revision 1.130  2002/08/17 23:41:34  mattias
  many clipping fixes

  Revision 1.129  2003/06/18 11:21:06  mattias
  fixed taborder=0, implemented TabOrder Editor

  Revision 1.128  2003/06/13 21:08:53  mattias
  moved TColorButton to dialogs.pp

  Revision 1.127  2003/06/13 14:38:01  mattias
  fixed using streamed clientwith/height for child anchors

  Revision 1.126  2003/06/13 12:53:51  mattias
  fixed TUpDown and added handler lists for TControl

  Revision 1.125  2003/06/11 22:29:42  mattias
  fixed realizing bounds after loading form

  Revision 1.124  2003/06/10 17:23:34  mattias
  implemented tabstop

  Revision 1.123  2003/06/10 12:28:23  mattias
  fixed anchoring controls

  Revision 1.122  2003/06/10 00:46:16  mattias
  fixed aligning controls

  Revision 1.121  2003/06/01 21:37:18  mattias
  fixed streaming TDataModule in programs

  Revision 1.120  2003/06/01 21:09:09  mattias
  implemented datamodules

  Revision 1.119  2003/05/30 16:25:47  mattias
  started datamodule

  Revision 1.118  2003/05/24 08:51:41  mattias
  implemented designer close query

  Revision 1.117  2003/05/09 14:21:25  mattias
  added published properties for gtkglarea

  Revision 1.116  2003/05/03 09:53:33  mattias
  fixed popupmenu for component palette

  Revision 1.115  2003/04/11 08:09:26  mattias
  published TControl help properties

  Revision 1.114  2003/04/07 01:59:25  mattias
  implemented package iterations

  Revision 1.113  2003/04/04 16:35:24  mattias
  started package registration

  Revision 1.112  2003/04/04 09:19:22  mattias
  activated TDataSource

  Revision 1.111  2003/04/02 13:23:23  mattias
  fixed default font

  Revision 1.110  2003/03/25 10:45:40  mattias
  reduced focus handling and improved focus setting

  Revision 1.109  2003/03/17 23:39:30  mattias
  added TCheckGroup

  Revision 1.108  2003/03/17 08:51:09  mattias
  added IsWindowVisible

  Revision 1.107  2003/03/11 23:14:19  mattias
  added TControl.HandleObjectShouldBeVisible

  Revision 1.106  2003/03/11 22:56:41  mattias
  added visiblechanging

  Revision 1.105  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.104  2003/03/09 17:44:12  mattias
  finshed Make Resourcestring dialog and implemented TToggleBox

  Revision 1.103  2003/02/27 09:52:00  mattias
  published TImgList.Width and Height

  Revision 1.102  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.101  2003/01/01 13:01:01  mattias
  fixed setcolor for streamed components

  Revision 1.100  2002/12/28 12:42:38  mattias
  focus fixes, reduced lpi size

  Revision 1.99  2002/12/27 18:18:05  mattias
  fixes for htmllite

  Revision 1.98  2002/12/27 17:46:04  mattias
  fixed SetColor

  Revision 1.97  2002/12/27 17:12:37  mattias
  added more Delphi win32 compatibility functions

  Revision 1.96  2002/12/25 10:21:05  mattias
  made Form.Close more Delphish, added some windows compatibility functions

  Revision 1.95  2002/12/18 17:52:18  mattias
  fixed lazarus xml files for fpc 1.1

  Revision 1.94  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.93  2002/12/04 20:39:14  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.92  2002/11/29 15:14:47  mattias
  replaced many invalidates by invalidaterect

  Revision 1.91  2002/11/21 18:49:52  mattias
  started OnMouseEnter and OnMouseLeave

  Revision 1.90  2002/11/09 15:02:06  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.89  2002/11/06 15:59:24  lazarus
  MG: fixed codetools abort

  Revision 1.88  2002/11/05 21:21:35  lazarus
  MG: fixed moving button with LEFT and RIGHT in messagedlgs

  Revision 1.87  2002/11/05 20:03:41  lazarus
  MG: implemented hints

  Revision 1.86  2002/11/04 19:49:35  lazarus
  MG: added persistent hints for main ide bar

  Revision 1.85  2002/11/03 22:40:28  lazarus
  MG: fixed ControlAtPos

  Revision 1.84  2002/11/01 14:40:30  lazarus
  MG: fixed mouse coords on scrolling wincontrols

  Revision 1.83  2002/10/30 12:37:25  lazarus
  MG: mouse cursors are now allocated on demand

  Revision 1.82  2002/10/26 15:15:45  lazarus
  MG: broke LCL<->interface circles

  Revision 1.81  2002/10/26 11:20:30  lazarus
  MG: broke some interfaces.pp circles

  Revision 1.80  2002/10/26 11:05:59  lazarus
  MG: broke actnlist <-> forms circle

  Revision 1.79  2002/10/24 10:05:50  lazarus
  MG: broke graphics.pp <-> clipbrd.pp circle

  Revision 1.78  2002/10/14 15:55:47  lazarus
  MG: reduced output

  Revision 1.77  2002/10/14 15:22:57  lazarus
  MG: default all hints to off

  Revision 1.76  2002/10/09 11:46:04  lazarus
  MG: fixed loading TListView from stream

  Revision 1.75  2002/10/01 10:41:47  lazarus
  MG: fixed mem leak

  Revision 1.74  2002/09/29 15:08:37  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>
    Patch includes:
      -fixes Problems with hiding modal forms
      -temporarily fixes TCustomForm.BorderStyle in bsNone
      -temporarily fixes problems with improper tabbing in TSynEdit

  Revision 1.73  2002/09/27 20:52:20  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.72  2002/09/10 06:49:18  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.71  2002/09/09 19:04:01  lazarus
  MG: started TTreeView dragging

  Revision 1.70  2002/09/09 14:01:05  lazarus
  MG: improved TScreen and ShowModal

  Revision 1.69  2002/09/08 10:01:59  lazarus
  MG: fixed streaming visible=false

  Revision 1.68  2002/09/06 22:32:20  lazarus
  Enabled cursor property + property editor.

  Revision 1.67  2002/09/05 12:11:42  lazarus
  MG: TNotebook is now streamable

  Revision 1.66  2002/09/03 08:07:17  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.65  2002/09/02 19:10:28  lazarus
  MG: TNoteBook now starts with no Page and TPage has no auto names

  Revision 1.64  2002/09/01 16:11:21  lazarus
  MG: double, triple and quad clicks now works

  Revision 1.63  2002/08/31 18:45:54  lazarus
  MG: added some property editors and started component editors

  Revision 1.62  2002/08/30 12:32:20  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.61  2002/08/30 06:46:03  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.60  2002/08/24 12:54:59  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.59  2002/08/23 19:00:15  lazarus
  MG: implemented Ctrl+Mouse links in source editor

  Revision 1.58  2002/08/22 16:22:39  lazarus
  MG: started debugging of mouse capturing

  Revision 1.57  2002/08/17 15:45:32  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.56  2002/08/07 09:55:29  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.55  2002/08/06 09:32:48  lazarus
  MG: moved TColor definition to graphtype.pp and registered TColor names

  Revision 1.54  2002/07/09 17:18:22  lazarus
  MG: fixed parser for external vars

  Revision 1.53  2002/06/21 15:41:56  lazarus
  MG: moved RectVisible, ExcludeClipRect and IntersectClipRect to interface dependent functions

  Revision 1.52  2002/06/19 19:46:08  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.51  2002/06/04 15:17:21  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.50  2002/05/30 21:19:26  lazarus

  + implemented HasParent for TControl & changed TCustomForm.GetChildren
    accordingly (sorry, control.inc & customform.inc got wrong comment:-( )
    stoppok

  Revision 1.49  2002/05/24 07:16:31  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.48  2002/05/20 14:19:03  lazarus
  MG: activated the clientrect bugfixes

  Revision 1.47  2002/05/10 06:05:49  lazarus
  MG: changed license to LGPL

  Revision 1.46  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.45  2002/05/06 08:50:36  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.44  2002/04/24 16:11:17  lazarus
  MG: started new client rectangle

  Revision 1.43  2002/04/24 09:29:06  lazarus
  MG: fixed typos

  Revision 1.42  2002/04/22 13:07:44  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.41  2002/04/21 06:53:54  lazarus
  MG: fixed save lrs to test dir

  Revision 1.40  2002/04/18 08:13:36  lazarus
  MG: added include comments

  Revision 1.39  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.38  2002/04/04 12:25:01  lazarus
  MG: changed except statements to more verbosity

  Revision 1.37  2002/03/31 23:20:37  lazarus
  MG: fixed initial size of TPage

  Revision 1.36  2002/03/29 17:12:52  lazarus
  MG: added Triple and Quad mouse clicks to lcl and synedit

  Revision 1.35  2002/03/25 17:59:19  lazarus
  GTK Cleanup
  Shane

  Revision 1.34  2002/03/16 21:40:54  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.33  2002/03/14 23:25:51  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

  Revision 1.32  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.31  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.30  2002/01/04 21:07:49  lazarus
  MG: added TTreeView

  Revision 1.29  2002/01/01 18:38:36  lazarus
  MG: more wmsize messages :(

  Revision 1.28  2002/01/01 15:50:13  lazarus
  MG: fixed initial component aligning

  Revision 1.27  2001/12/08 08:54:45  lazarus
  MG: added TControl.Refresh

  Revision 1.26  2001/12/05 17:23:44  lazarus
  Added Calendar component
  Shane

  Revision 1.25  2001/11/10 10:48:00  lazarus
  MG: fixed set formicon on invisible forms

  Revision 1.24  2001/11/09 19:14:23  lazarus
  HintWindow changes
  Shane

  Revision 1.23  2001/10/31 16:29:21  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.22  2001/10/07 07:28:32  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.21  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.20  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.19  2001/05/13 22:07:08  lazarus
  Implemented BringToFront / SendToBack.

  Revision 1.18  2001/03/27 21:12:53  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.17  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.16  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.14  2001/03/12 12:17:01  lazarus
  MG: fixed random function results

  Revision 1.13  2001/02/20 16:53:27  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.12  2001/02/04 04:18:12  lazarus
  Code cleanup and JITFOrms bug fix.
  Shane

  Revision 1.11  2001/02/01 16:45:19  lazarus
  Started the code completion.
  Shane

  Revision 1.10  2001/01/23 23:33:54  lazarus
  MWE:
    - Removed old LM_InvalidateRect
    - did some cleanup in old  code
    + added some comments  on gtkobject data (gtkproc)

  Revision 1.9  2000/12/29 13:14:05  lazarus
  Using the lresources.pp and registering components.
  This is a major change but will create much more flexibility for the IDE.
  Shane

  Revision 1.8  2000/12/22 19:55:37  lazarus
  Added the Popupmenu code to the LCL.
  Now you can right click on the editor and a PopupMenu appears.
  Shane

  Revision 1.7  2000/12/20 17:35:58  lazarus
  Added GetChildren
  Shane

  Revision 1.6  2000/12/01 15:50:39  lazarus
  changed the TCOmponentInterface SetPropByName.  It works for a few properties, but not all.
  Shane

  Revision 1.5  2000/11/30 21:43:38  lazarus
  Changed TDesigner.  It's now notified when a control is added to it's CustomForm.
  It's created in main.pp when New Form is selected.

  Shane

  Revision 1.3  2000/11/27 18:52:37  lazarus
  Added the Object Inspector code.
  Added more form editor code.
  Shane

  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.92  2000/07/09 20:18:55  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.91  2000/06/28 13:11:37  lazarus
  Fixed TNotebook so it gets page change events.  Shane

  Revision 1.90  2000/06/16 13:33:21  lazarus
  Created a new method for adding controls to the toolbar to be dropped onto the form!
  Shane

  Revision 1.89  2000/05/27 22:20:55  lazarus
  MWE & VRS:
    + Added new hint code

  Revision 1.88  2000/05/23 21:41:10  lazarus
  MWE:
    * Fixed (one ?) crash on close: Mouse is created/freed twice.
      Thanks to Vincent Snijders pointing at this.

  Revision 1.87  2000/05/14 21:56:11  lazarus
  MWE:
    + added local messageloop
    + added PostMessage
    * fixed Peekmessage
    * fixed ClientToScreen
    * fixed Flat style of Speedutton (TODO: Draw)
    + Added TApplicatio.OnIdle

  Revision 1.86  2000/05/10 22:52:57  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.85  2000/05/09 18:37:02  lazarus
  *** empty log message ***

  Revision 1.84  2000/05/09 12:52:02  lazarus
  *** empty log message ***

  Revision 1.83  2000/05/09 00:38:10  lazarus
  Changed writelns to Asserts.                          CAW

  Revision 1.82  2000/05/08 16:07:32  lazarus
  fixed screentoclient and clienttoscreen
  Shane

  Revision 1.80  2000/04/18 21:03:13  lazarus
  Added
  TControl.bringtofront
  Shane

  Revision 1.79  2000/04/18 14:02:32  lazarus
  Added Double Clicks.  Changed the callback in gtkcallback for the buttonpress event to check the event type.
  Shane

  Revision 1.78  2000/04/17 19:50:05  lazarus
  Added some compiler stuff built into Lazarus.
  This depends on the path to your compiler being correct in the compileroptions
  dialog.
  Shane

  Revision 1.77  2000/04/13 21:25:16  lazarus
  MWE:
    ~ Added some docu and did some cleanup.
  Hans-Joachim Ott <hjott@compuserve.com>:
    * TMemo.Lines works now.
    + TMemo has now a property Scrollbar.
    = TControl.GetTextBuf revised :-)
    + Implementation for CListBox columns added
    * Bug in TGtkCListStringList.Assign corrected.

  Revision 1.76  2000/04/10 15:05:30  lazarus
  Modified the way the MOuseCapture works.
  Shane

  Revision 1.74  2000/04/07 16:59:54  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.73  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.72  2000/03/22 20:40:43  lazarus
  Added dragobject shell

  Revision 1.71  2000/03/20 20:08:33  lazarus
  Added a generic MOUSE class.
  Shane

  Revision 1.70  2000/03/15 20:15:31  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.69  2000/03/15 00:51:57  lazarus
  MWE:
    + Added LM_Paint on expose
    + Added forced creation of gdkwindow if needed
    ~ Modified DrawFrameControl
    + Added BF_ADJUST support on DrawEdge
    - Commented out LM_IMAGECHANGED in TgtkObject.IntSendMessage3
       (It did not compile)

  Revision 1.68  2000/03/14 19:49:04  lazarus
  Modified the painting process for TWincontrol.  Now it runs throug it's FCONTROLS list and paints all them
  Shane

  Revision 1.67  2000/03/10 18:31:09  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.66  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.65  2000/02/28 00:15:54  lazarus
  MWE:
    Fixed creation of visible componets at runtime. (when a new editor
      was created it didn't show up)
    Made the hiding/showing of controls more delphi compatible

  Revision 1.64  2000/02/24 21:15:30  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.63  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.62  2000/02/22 21:51:40  lazarus
  MWE: Removed some double (or triple) event declarations.
       The latest compiler doesn't like it

  Revision 1.61  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.60  2000/02/19 18:11:59  lazarus
  More work on moving, resizing, forms' border style etc.

  Revision 1.59  2000/02/18 19:38:52  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.58  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.57  2000/01/10 00:07:12  lazarus
  MWE:
    Added more scrollbar support for TWinControl
    Most signals for TWinContorl are jet connected to the wrong widget
      (now scrolling window, should be fixed)
    Added some cvs entries


}


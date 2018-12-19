{  $Id$  }
{
 /***************************************************************************
                               Controls.pp
                             -------------------
                             Component Library Controls
                   Initial Revision : Sat Apr 10 22:49:32 CST 1999


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Controls;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}
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

{$INTERFACES CORBA}

uses
  Classes, SysUtils, TypInfo, Types, Laz_AVL_Tree,
  // LCL
  LCLStrConsts, LCLType, LCLProc, GraphType, Graphics, LMessages, LCLIntf,
  InterfaceBase, ImgList, PropertyStorage, Menus, ActnList, LCLClasses,
  LResources, LCLPlatformDef,
  // LazUtils
  LazMethodList, LazLoggerBase, LazUtilities, UITypes;

{$I controlconsts.inc}

const
  // Used for ModalResult
  mrNone    = UITypes.mrNone;
  mrOK      = UITypes.mrOK;
  mrCancel  = UITypes.mrCancel;
  mrAbort   = UITypes.mrAbort;
  mrRetry   = UITypes.mrRetry;
  mrIgnore  = UITypes.mrIgnore;
  mrYes     = UITypes.mrYes;
  mrNo      = UITypes.mrNo;
  mrAll     = UITypes.mrAll;
  mrNoToAll = UITypes.mrNoToAll;
  mrYesToAll= UITypes.mrYesToAll;
  mrClose   = UITypes.mrClose;
  mrLast    = UITypes.mrLast;

function GetModalResultStr(ModalResult: TModalResult): ShortString;
  deprecated 'Use the ModalResultStr array from unit UITypes directly.';
property ModalResultStr[ModalResult: TModalResult]: shortstring read GetModalResultStr;

const
  // define aliases for Delphi compatibility
  fsSurface = GraphType.fsSurface;
  fsBorder = GraphType.fsBorder;

  bvNone = GraphType.bvNone;
  bvLowered = GraphType.bvLowered;
  bvRaised = GraphType.bvRaised;
  bvSpace = GraphType.bvSpace;

  // Constant to define which key should be utilized for keyboard shortcuts like Ctrl+C (Copy),Z,X,V
  // Mac and iOS use Meta instead of Ctrl for those shortcuts
  ssModifier = {$if defined(darwin) or defined(macos) or defined(iphonesim)} ssMeta {$else} ssCtrl {$endif};

  GUID_ObjInspInterface = '{37417989-8C8F-4A2D-9D26-0FA377E8D8CC}';

type
  IObjInspInterface = interface
      [GUID_ObjInspInterface]
      function AllowAdd: Boolean;
      function AllowDelete: Boolean;
    end;

  TWinControl = class;
  TControl = class;
  TWinControlClass = class of TWinControl;
  TControlClass = class of TControl;

  // ToDo: move this to a message definition unit
  TCMMouseWheel = record
    MSg: Cardinal;
    ShiftState: TShiftState;
    Unused: Byte;
    WheelDelta: SmallInt;
    case Integer of
    0: (
      XPos: SmallInt;
      YPos: SmallInt);
    1: (
      Pos: TSmallPoint;
      Result: LRESULT);
  end;

  TCMHitTest = TLMNCHitTest;
  TCMDesignHitTest = TLMMouse;

  TCMControlChange = record
    Msg: Cardinal;
    Control: TControl;
    Inserting: LongBool;
    Result: LRESULT;
  end;

  TCMChanged = record
    Msg: Cardinal;
    Unused: Longint;
    Child: TControl;
    Result: Longint;
  end;

  TCMControlListChange = record
    Msg: Cardinal;
    Control: TControl;
    Inserting: LongBool;
    Result: LRESULT;
  end;

  TCMDialogChar = TLMKEY;
  TCMDialogKey = TLMKEY;

  TCMEnter = TLMEnter;
  TCMExit = TLMExit;

  TCMCancelMode = record
    Msg: Cardinal;
    Unused: Integer;
    Sender: TControl;
    Result: Longint;
  end;

  TCMChildKey = record
    Msg: Cardinal;
{$ifdef cpu64}
    UnusedMsg: Cardinal;
{$endif}
{$IFDEF FPC_LITTLE_ENDIAN}
    CharCode: Word; // VK_XXX constants as TLMKeyDown/Up, ascii if TLMChar
    Unused: Word;
{$ELSE}
    Unused: Word;
    CharCode: Word; // VK_XXX constants as TLMKeyDown/Up, ascii if TLMChar
{$ENDIF}
{$ifdef cpu64}
    Unused2 : Longint;
{$endif cpu64}
    Sender: TWinControl;
    Result: LRESULT;
  end;

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

  TFormStyle = (fsNormal, fsMDIChild, fsMDIForm, fsStayOnTop, fsSplash, fsSystemStayOnTop);
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

  TMouseButton = (mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2);

const
  fsAllStayOnTop = [fsStayOnTop, fsSystemStayOnTop];
  fsAllNonSystemStayOnTop = [fsStayOnTop];

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
    csAcceptsControls,       // can have children in the designer
    csCaptureMouse,          // auto capture mouse when clicked
    csDesignInteractive,     // wants mouse events in design mode
    csClickEvents,           // handles mouse events
    csFramed,                // not implemented, has 3d frame
    csSetCaption,            // if Name=Caption, changing the Name changes the Caption
    csOpaque,                // the control paints its area completely
    csDoubleClicks,          // understands mouse double clicks
    csTripleClicks,          // understands mouse triple clicks
    csQuadClicks,            // understands mouse quad clicks
    csFixedWidth,            // cannot change its width
    csFixedHeight,           // cannot change its height (for example combobox)
    csNoDesignVisible,       // is invisible in the designer
    csReplicatable,          // PaintTo works
    csNoStdEvents,           // standard events such as mouse, key, and click events are ignored.
    csDisplayDragImage,      // display images from dragimagelist during drag operation over control
    csReflector,             // not implemented, the controls respond to size, focus and dlg messages - it can be used as ActiveX control under Windows
    csActionClient,          // Action is set
    csMenuEvents,            // not implemented
    csNoFocus,               // control will not take focus when clicked with mouse.
    csNeedsBorderPaint,      // not implemented
    csParentBackground,      // tells WinXP to paint the theme background of parent on controls background
    csDesignNoSmoothResize,  // when resizing control in the designer do not SetBounds while dragging
    csDesignFixedBounds,     // can not be moved nor resized in designer
    csHasDefaultAction,      // implements useful ExecuteDefaultAction
    csHasCancelAction,       // implements useful ExecuteCancelAction
    csNoDesignSelectable,    // can not be selected at design time
    csOwnedChildrenNotSelectable, // child controls owned by this control are NOT selectable in the designer
    csAutoSize0x0,           // if the preferred size is 0x0 then control is shrinked ot 0x0
    csAutoSizeKeepChildLeft, // when AutoSize=true do not move children horizontally
    csAutoSizeKeepChildTop,  // when AutoSize=true do not move children vertically
    csRequiresKeyboardInput  // If the device has no physical keyboard then show the virtual keyboard when this control gets focus (therefore available only to TWinControl descendents)
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
    csFocusing,
    csCreating, // not used, exists for Delphi compatibility
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
  protected
    procedure CreateHandle; override;
    function GetDefaultColor(const ADefaultColorType: TDefaultColorType): TColor; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeHandle;override;
    function ControlIsPainting: boolean;
    property Control: TControl read FControl write SetControl;
  end;

  { Hint stuff }

  PHintInfo = ^THintInfo;
  THintInfo = record
    HintControl: TControl;
    HintWindowClass: TWinControlClass;
    HintPos: TPoint; // screen coordinates
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

  TImageListHelper = class helper for TCustomImageList
  private
    function GetResolutionForControl(AImageWidth: Integer; AControl: TControl): TScaledImageListResolution;
  public
    procedure DrawForControl(ACanvas: TCanvas; AX, AY, AIndex, AImageWidthAt96PPI: Integer;
      AControl: TControl; AEnabled: Boolean = True); overload;
    procedure DrawForControl(ACanvas: TCanvas; AX, AY, AIndex, AImageWidthAt96PPI: Integer;
      AControl: TControl; ADrawEffect: TGraphicsDrawEffect); overload;

    property ResolutionForControl[AImageWidth: Integer; AControl: TControl]: TScaledImageListResolution read GetResolutionForControl;
  end;

  TDragImageList = class;

  TDragImageListResolution = class(TCustomImageListResolution)
  private
    FDragging: Boolean;
    FDragHotspot: TPoint;
    FOldCursor: TCursor;
    FLastDragPos: TPoint;
    FLockedWindow: HWND;// window where drag started and locked via DragLock, invalid=NoLockedWindow=High(PtrInt)

    function GetImageList: TDragImageList;
  protected
    class procedure WSRegisterClass; override;

    property ImageList: TDragImageList read GetImageList;
  public
    constructor Create(TheOwner: TComponent); override;

    function GetHotSpot: TPoint; override;
    function BeginDrag(Window: HWND; X, Y: Integer): Boolean;
    function DragLock(Window: HWND; XPos, YPos: Integer): Boolean;
    function DragMove(X, Y: Integer): Boolean;
    procedure DragUnlock;
    function EndDrag: Boolean;
    procedure HideDragImage;
    procedure ShowDragImage;

    property DragHotspot: TPoint read FDragHotspot write FDragHotspot;
    property Dragging: Boolean read FDragging;
  end;

  TDragImageList = class(TCustomImageList)
  private
    FDragCursor: TCursor;
    FImageIndex: Integer;
    procedure SetDragCursor(const AValue: TCursor);
    function GetResolution(AImageWidth: Integer): TDragImageListResolution;
    function GetDragging: Boolean;
    function GetDraggingResolution: TDragImageListResolution;
    function GetDragHotspot: TPoint;
    procedure SetDragHotspot(const aDragHotspot: TPoint);
  protected
    function GetResolutionClass: TCustomImageListResolutionClass; override;
    procedure Initialize; override;
  public
    function BeginDrag(Window: HWND; X, Y: Integer): Boolean;
    function DragLock(Window: HWND; XPos, YPos: Integer): Boolean;
    function DragMove(X, Y: Integer): Boolean;
    procedure DragUnlock;
    function EndDrag: Boolean;
    procedure HideDragImage;
    function SetDragImage(Index, HotSpotX, HotSpotY: Integer): Boolean;
    procedure ShowDragImage;
    property DragCursor: TCursor read FDragCursor write SetDragCursor;
    property DragHotspot: TPoint read GetDragHotspot write SetDragHotspot;
    property Dragging: Boolean read GetDragging;
    property DraggingResolution: TDragImageListResolution read GetDraggingResolution;
    property Resolution[AImageWidth: Integer]: TDragImageListResolution read GetResolution;
  end;

  TKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of Object;
  TKeyPressEvent = procedure(Sender: TObject; var Key: char) of Object;
  TUTF8KeyPressEvent = procedure(Sender: TObject; var UTF8Key: TUTF8Char) of Object;

  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer) of Object;
  TMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
                              X, Y: Integer) of Object;
  TMouseWheelEvent = procedure(Sender: TObject; Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) of Object;
  TMouseWheelUpDownEvent = procedure(Sender: TObject;
          Shift: TShiftState; MousePos: TPoint; var Handled: Boolean) of Object;

  TGetDockCaptionEvent = procedure(Sender: TObject; AControl: TControl;
    var ACaption: String) of Object;


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
    FAlwaysShowDragImages: Boolean;
    FDragPos: TPoint;
    FControl: TControl;
    FDragTarget: TControl;
    FDragTargetPos: TPoint;
    FAutoFree: Boolean;
    FAutoCreated: Boolean;
    FDropped: Boolean;
  protected
    procedure EndDrag(Target: TObject; X, Y: Integer); virtual;
    function GetDragImages: TDragImageList; virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; virtual;
  public
    constructor Create(AControl: TControl); virtual;
    constructor AutoCreate(AControl: TControl);

    procedure HideDragImage; virtual;
    procedure ShowDragImage; virtual;

    property AlwaysShowDragImages: Boolean read FAlwaysShowDragImages write FAlwaysShowDragImages;
    property AutoCreated: Boolean read FAutoCreated;
    property AutoFree: Boolean read FAutoFree;
    property Control: TControl read FControl write FControl; // the dragged control
    property DragPos: TPoint read FDragPos write FDragPos;
    property DragTarget: TControl read FDragTarget write FDragTarget;
    property DragTargetPos: TPoint read FDragTargetPos write FDragTargetPos;
    property Dropped: Boolean read FDropped;
  end;

  TDragObjectClass = class of TDragObject;

  { TDragObjectEx }

  TDragObjectEx = class(TDragObject)
  public
    constructor Create(AControl: TControl); override;
  end;


  { TDragControlObject }

  TDragControlObject = class(TDragObject)
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    function GetDragImages: TDragImageList; override;
  end;

  { TDragControlObjectEx }

  TDragControlObjectEx = class(TDragControlObject)
  public
    constructor Create(AControl: TControl); override;
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

  TDrawDockImageEvent = procedure(Sender: TObject; AOldRect, ANewRect: TRect; AOperation: TDockImageOperation);

var
  OnDrawDockImage: TDrawDockImageEvent = nil;

type
  TDragDockObject = class(TDragObject)
  private
    FDockOffset: TPoint;
    FDockRect: TRect;
    FDropAlign: TAlign;
    FDropOnControl: TControl;
    FEraseDockRect: TRect;
    FFloating: Boolean;
    FIncreaseDockArea: Boolean;
  protected
    procedure AdjustDockRect(ARect: TRect); virtual;
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
    procedure EndDrag(Target: TObject; X, Y: Integer); override;

    // dock image drawing
    procedure InitDock(APosition: TPoint); virtual;
    procedure ShowDockImage; virtual;
    procedure HideDockImage; virtual;
    procedure MoveDockImage; virtual;
    function HasOnDrawImage: boolean; virtual;
  public
    property DockOffset: TPoint read FDockOffset write FDockOffset;
    property DockRect: TRect read FDockRect write FDockRect; // where to drop Control, screen coordinates
    property DropAlign: TAlign read FDropAlign write FDropAlign; // how to align Control
    property DropOnControl: TControl read FDropOnControl write FDropOnControl; // drop on child control of Target (Target is a parameter, not a property)
    property Floating: Boolean read FFloating write FFloating;
    property IncreaseDockArea: Boolean read FIncreaseDockArea;
    property EraseDockRect: TRect read FEraseDockRect write FEraseDockRect;
  end;

  { TDragDockObjectEx }

  TDragDockObjectEx = class(TDragDockObject)
  public
    constructor Create(AControl: TControl); override;
  end;

  { TDragManager }

  TDragManager = class(TComponent)
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
    constructor Create(TheOwner: TComponent); override;

    function IsDragging: boolean; virtual;abstract;
    function Dragging(AControl: TControl): boolean; virtual;abstract;
    procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean); virtual;abstract;

    procedure DragStart(AControl: TControl; AImmediate: Boolean; AThreshold: Integer; StartFromCurrentMouse:Boolean=False);virtual;abstract;
    procedure DragMove(APosition: TPoint); virtual;abstract;
    procedure DragStop(ADrop: Boolean); virtual;abstract;

    function CanStartDragging(Site: TWinControl;  AThreshold: Integer; X,Y:Integer): boolean; virtual;abstract;

    property DragImmediate: Boolean read FDragImmediate write FDragImmediate default True;
    property DragThreshold: Integer read FDragThreshold write FDragThreshold default 5;
  end;

var
  DragManager: TDragManager = nil;// created in initialization

type
  { TDockManager is an abstract class for managing a dock site's docked
    controls. See TDockTree below for more info.
    }
  TDockManager = class(TPersistent)
  public
    constructor Create(ADockSite: TWinControl); virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure GetControlBounds(Control: TControl;
                               out AControlBounds: TRect); virtual; abstract;
    function GetDockEdge(ADockObject: TDragDockObject): boolean; virtual;
    procedure InsertControl(ADockObject: TDragDockObject); virtual; overload;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
                            DropCtl: TControl); virtual; abstract; overload;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure PaintSite(DC: HDC); virtual;
    procedure MessageHandler(Sender: TControl; var Message: TLMessage); virtual;
    procedure PositionDockRect(ADockObject: TDragDockObject); virtual; overload;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
                               var DockRect: TRect); virtual; abstract; overload;
    procedure RemoveControl(Control: TControl); virtual; abstract;
    procedure ResetBounds(Force: Boolean); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure SetReplacingControl(Control: TControl); virtual;
    function AutoFreeByControl: Boolean; virtual;
    function IsEnabledControl(Control: TControl):Boolean; virtual;
  end;

  TDockManagerClass = class of TDockManager;

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
    procedure Change; virtual;
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
    procedure AutoAdjustLayout(const AXProportion, AYProportion: Double);
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
    The spacing around its children and between its children is defined in
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

  TSpacingSize = Integer;
  TControlCellAlign = (
    ccaFill,
    ccaLeftTop,
    ccaRightBottom,
    ccaCenter
    );
  TControlCellAligns = set of TControlCellAlign;

  { TControlBorderSpacingDefault defines the default values for TControlBorderSpacing
    so derived TControl classes can define their own default values }

  TControlBorderSpacingDefault = record
    Left: TSpacingSize;
    Top: TSpacingSize;
    Right: TSpacingSize;
    Bottom: TSpacingSize;
    Around: TSpacingSize;
  end;
  PControlBorderSpacingDefault = ^TControlBorderSpacingDefault;


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
    FDefault: PControlBorderSpacingDefault;
    function GetAroundBottom: Integer;
    function GetAroundLeft: Integer;
    function GetAroundRight: Integer;
    function GetAroundTop: Integer;
    function GetControlBottom: Integer;
    function GetControlHeight: Integer;
    function GetControlLeft: Integer;
    function GetControlRight: Integer;
    function GetControlTop: Integer;
    function GetControlWidth: Integer;
    function IsAroundStored: boolean;
    function IsBottomStored: boolean;
    function IsInnerBorderStored: boolean;
    function IsLeftStored: boolean;
    function IsRightStored: boolean;
    function IsTopStored: boolean;
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
    procedure Change(InnerSpaceChanged: Boolean); virtual;
  public
    constructor Create(OwnerControl: TControl; ADefault: PControlBorderSpacingDefault = nil);
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function IsEqual(Spacing: TControlBorderSpacing): boolean;
    procedure GetSpaceAround(var SpaceAround: TRect); virtual;
    function GetSideSpace(Kind: TAnchorKind): Integer; // Around+GetSpace
    function GetSpace(Kind: TAnchorKind): Integer; virtual;
    procedure AutoAdjustLayout(const AXProportion, AYProportion: Double);
  public
    property Control: TControl read FControl;
    property Space[Kind: TAnchorKind]: integer read GetSpace write SetSpace;
    property AroundLeft: Integer read GetAroundLeft;
    property AroundTop: Integer read GetAroundTop;
    property AroundRight: Integer read GetAroundRight;
    property AroundBottom: Integer read GetAroundBottom;
    property ControlLeft: Integer read GetControlLeft;
    property ControlTop: Integer read GetControlTop;
    property ControlWidth: Integer read GetControlWidth;
    property ControlHeight: Integer read GetControlHeight;
    property ControlRight: Integer read GetControlRight;
    property ControlBottom: Integer read GetControlBottom;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Left: TSpacingSize read FLeft write SetLeft stored IsLeftStored;
    property Top: TSpacingSize read FTop write SetTop stored IsTopStored;
    property Right: TSpacingSize read FRight write SetRight stored IsRightStored;
    property Bottom: TSpacingSize read FBottom write SetBottom stored IsBottomStored;
    property Around: TSpacingSize read FAround write SetAround stored IsAroundStored;
    property InnerBorder: Integer read FInnerBorder write SetInnerBorder stored IsInnerBorderStored default 0;
    property CellAlignHorizontal: TControlCellAlign read FCellAlignHorizontal write SetCellAlignHorizontal default ccaFill;
    property CellAlignVertical: TControlCellAlign read FCellAlignVertical write SetCellAlignVertical default ccaFill;
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
    FKind: TAnchorKind;
    FControl: TControl;
    FOwner: TControl;
    FSide: TAnchorSideReference;
    function IsSideStored: boolean;
    procedure SetControl(const AValue: TControl);
    procedure SetSide(const AValue: TAnchorSideReference);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(TheOwner: TControl; TheKind: TAnchorKind);
    destructor Destroy; override;
    procedure GetSidePosition(out ReferenceControl: TControl;
                out ReferenceSide: TAnchorSideReference; out Position: Integer);
    function CheckSidePosition(NewControl: TControl; NewSide: TAnchorSideReference;
                out ReferenceControl: TControl;
                out ReferenceSide: TAnchorSideReference; out Position: Integer): boolean;
    procedure Assign(Source: TPersistent); override;
    function IsAnchoredToParent(ParentSide: TAnchorKind): boolean;
    procedure FixCenterAnchoring;
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
    procedure SetCaption(const Value: string); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: String); override;
    procedure SetHelpContext(Value: THelpContext); override;
    procedure SetHelpKeyword(const Value: string); override;
    procedure SetHelpType(Value: THelpType); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
    function IsOnExecuteLinked: Boolean; override;
    function DoShowHint(var HintStr: string): Boolean; virtual;
  public
    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHelpLinked: Boolean;  override;
    function IsHintLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
  end;

  TControlActionLinkClass = class of TControlActionLink;


  { TControl }

  ELayoutException = class(Exception);

  TControlAutoSizePhase = (
    caspNone,
    caspChangingProperties,
    caspCreatingHandles, // create/destroy handles
    caspComputingBounds,
    caspRealizingBounds,
    caspShowing          // make handles visible
    );
  TControlAutoSizePhases = set of TControlAutoSizePhase;

  TTabOrder = -1..32767;

  TControlShowHintEvent = procedure(Sender: TObject; HintInfo: PHintInfo) of object;
  TContextPopupEvent = procedure(Sender: TObject; MousePos: TPoint;
                                 var Handled: Boolean) of object;

  TControlFlag = (
    cfLoading, // set by TControl.ReadState, unset by TControl.Loaded when all on form finished loading
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
    cfOnChangeBoundsNeeded,
    cfProcessingWMPaint,
    cfKillChangeBounds,
    cfKillInvalidatePreferredSize,
    cfKillAdjustSize
    );
  TControlFlags = set of TControlFlag;

  TControlHandlerType = (
    chtOnResize,
    chtOnChangeBounds,
    chtOnVisibleChanging,
    chtOnVisibleChanged,
    chtOnEnabledChanging,
    chtOnEnabledChanged,
    chtOnKeyDown,
    chtOnBeforeDestruction,
    chtOnMouseWheel,
    chtOnMouseWheelHorz
    );

  TLayoutAdjustmentPolicy = (
    lapDefault,     // widgetset dependent
    lapFixedLayout, // A fixed absolute layout in all platforms
    lapAutoAdjustWithoutHorizontalScrolling, // Smartphone platforms use this one,
                                             // the x axis is stretched to fill the screen and
                                             // the y is scaled to fit the DPI
    lapAutoAdjustForDPI // For desktops using High DPI, scale x and y to fit the DPI
  );

  TLazAccessibilityRole = (
    larAnimation, // An object that displays an animation.
    larButton, // A button.
    larCell, // A cell in a table.
    larChart, // An object that displays a graphical representation of data.
    larCheckBox, // An object that can be checked or unchecked, or sometimes in an intermediary state
    larClock, // A clock displaying time.
    larColorPicker, // A control which allows selecting a color.
    larComboBox, // A list of choices that the user can select from.
    larDateField, // A controls which displays and possibly allows one to choose a date.
    larGrid, // A grid control which displays cells
    larGroup, // A control which groups others, such as a TGroupBox.
    larIgnore, // Something to be ignored. For example a blank space between other objects.
    larImage, // A graphic or picture or an icon.
    larLabel, // A text label as usually placed near other widgets.
    larListBox, // A list of items, from which the user can select one or more items.
    larListItem, // An item in a list of items.
    larMenuBar, // A main menu bar.
    larMenuItem, // A item in a menu.
    larProgressIndicator, // A control which shows a progress indication.
    larRadioButton, // A radio button, see for example TRadioButton.
    larResizeGrip, // A grip that the user can drag to change the size of widgets.
    larScrollBar, // A control to scroll another one
    larSpinner, // A control which allows one to increment / decrement a value.
    larTabControl, // A control with tabs, like TPageControl.
    larTextEditorMultiline, // A multi-line text editor (for example: TMemo, SynEdit)
    larTextEditorSingleline, // A single-line text editor (for example: TEdit)
    larTrackBar, // A control which allows one to drag a slider.
    larTreeView, // A list of items in a tree structure.
    larTreeItem, // An item in a tree structure.
    larWindow // A top level window.
  );

  // The Child Accessible Objects are designed for non-TControl children
  // of a TCustomControl descendent, for example the items of a TTreeView

  TLazAccessibleObject = class;

  { TLazAccessibleObjectEnumerator }

  TLazAccessibleObjectEnumerator = class(TAvlTreeNodeEnumerator)
  private
    function GetCurrent: TLazAccessibleObject;
  public
    property Current: TLazAccessibleObject read GetCurrent;
  end;

  { TLazAccessibleObject }

  TLazAccessibleObject = class
  private
    FHandle: PtrInt;
    FPosition: TPoint;
    FSize: TSize;
    // only for GetChildAccessibleObject(Index)
    FLastSearchNode: TAvlTreeNode;
    FLastSearchIndex: Integer;
    FLastSearchInSubcontrols: Boolean;
    function GetHandle: PtrInt;
    function GetPosition: TPoint;
    function GetSize: TSize;
    procedure SetHandle(AValue: PtrInt);
    procedure SetPosition(AValue: TPoint);
    procedure SetSize(AValue: TSize);
  protected
    FChildrenSortedForDataObject: TAvlTree; // of TLazAccessibleObject
    FAccessibleDescription: TCaption;
    FAccessibleValue: TCaption;
    FAccessibleRole: TLazAccessibilityRole;
    class procedure WSRegisterClass; virtual;//override;
    // provided for descendents to override and implement
    function GetAccessibleValue: TCaption; virtual;
  public
    OwnerControl: TControl;
    Parent: TLazAccessibleObject;
    DataObject: TObject; // Available to be used to connect to an object
    SecondaryHandle: PtrInt; // Available for Widgetsets to use
    constructor Create(AOwner: TControl); virtual;
    destructor Destroy; override;
    function HandleAllocated: Boolean;
    procedure InitializeHandle; virtual;
    procedure SetAccessibleDescription(const ADescription: TCaption);
    procedure SetAccessibleValue(const AValue: TCaption);
    procedure SetAccessibleRole(const ARole: TLazAccessibilityRole);
    function FindOwnerWinControl: TWinControl;
    function AddChildAccessibleObject: TLazAccessibleObject; virtual;
    procedure InsertChildAccessibleObject(AObject: TLazAccessibleObject);
    procedure ClearChildAccessibleObjects;
    procedure RemoveChildAccessibleObject(AObject: TLazAccessibleObject; AFreeObject: Boolean = True);
    // These search only in the child objects added manually
    function GetChildAccessibleObjectWithDataObject(ADataObject: TObject): TLazAccessibleObject;
    function GetChildAccessibleObjectsCount: Integer;
    function GetChildAccessibleObject(AIndex: Integer): TLazAccessibleObject;
    // These search in all subcontrols too
    function GetFirstChildAccessibleObject: TLazAccessibleObject;
    function GetNextChildAccessibleObject: TLazAccessibleObject;
    //
    function GetSelectedChildAccessibleObject: TLazAccessibleObject; virtual;
    function GetChildAccessibleObjectAtPos(APos: TPoint): TLazAccessibleObject; virtual;
    // Primary information
    property AccessibleDescription: TCaption read FAccessibleDescription write SetAccessibleDescription;
    property AccessibleValue: TCaption read GetAccessibleValue write SetAccessibleValue;
    property AccessibleRole: TLazAccessibilityRole read FAccessibleRole write SetAccessibleRole;
    property Position: TPoint read GetPosition write SetPosition;
    property Size: TSize read GetSize write SetSize;
    property Handle: PtrInt read GetHandle write SetHandle;
    function GetEnumerator: TLazAccessibleObjectEnumerator;
  end;

{* Note on TControl.Caption
 * The VCL implementation relies on the virtual Get/SetTextBuf to
 * exchange text between widgets and VCL. This means a lot of
 * (unnecessary) text copies.
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
    FAnchoredControls: TFPList; // list of TControl anchored to this control
    FAutoSizingLockCount: Integer; // in/decreased by DisableAutoSizing/EnableAutoSizing
    {$IFDEF DebugDisableAutoSizing}
    FAutoSizingLockReasons: TStrings;
    {$ENDIF}
    FBaseBounds: TRect;
    FBaseBoundsLock: integer;
    FBaseParentClientSize: TSize;
    FBiDiMode: TBiDiMode;
    FBorderSpacing: TControlBorderSpacing;
    FBoundsRectForNewParent: TRect;
    FCaption: TCaption;
    FCaptureMouseButtons: TCaptureMouseButtons;
    FColor: TColor;
    FConstraints: TSizeConstraints;
    FControlFlags: TControlFlags;
    FControlHandlers: array[TControlHandlerType] of TMethodList;
    FControlStyle: TControlStyle;
    FDesktopFont: Boolean;
    FDockOrientation: TDockOrientation;
    FDragCursor: TCursor;
    FDragKind: TDragKind;
    FDragMode: TDragMode;
    FFloatingDockSiteClass: TWinControlClass;
    FFont: TFont;
    FHeight: Integer;
    FHelpContext: THelpContext;
    FHelpKeyword: String;
    FHelpType: THelpType;
    FHint: TTranslateString;
    FHostDockSite: TWinControl;
    FLastDoChangeBounds: TRect;
    FLastDoChangeClientSize: TPoint;
    FLastResizeClientHeight: integer;
    FLastResizeClientWidth: integer;
    FLastResizeHeight: integer;
    FLastResizeWidth: integer;
    FLeft: Integer;
    FLoadedClientSize: TSize;
    FLRDockWidth: Integer;
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
    FOnMouseWheel: TMouseWheelEvent;
    FOnMouseWheelDown: TMouseWheelUpDownEvent;
    FOnMouseWheelUp: TMouseWheelUpDownEvent;
    FOnMouseWheelHorz: TMouseWheelEvent;
    FOnMouseWheelLeft: TMouseWheelUpDownEvent;
    FOnMouseWheelRight: TMouseWheelUpDownEvent;
    FOnQuadClick: TNotifyEvent;
    FOnResize: TNotifyEvent;
    FOnShowHint: TControlShowHintEvent;
    FOnStartDock: TStartDockEvent;
    FOnStartDrag: TStartDragEvent;
    FOnTripleClick: TNotifyEvent;
    FParent: TWinControl;
    FParentBiDiMode: Boolean;
    FPopupMenu: TPopupMenu;
    FPreferredMinWidth: integer;// without theme space
    FPreferredMinHeight: integer;// without theme space
    FPreferredWidth: integer;// with theme space
    FPreferredHeight: integer;// with theme space
    FReadBounds: TRect;
    FSessionProperties: string;
    FSizeLock: integer;
    FTBDockHeight: Integer;
    FTop: Integer;
    FUndockHeight: Integer;
    FUndockWidth: Integer;
    FWidth: Integer;
    FWindowProc: TWndMethod;
    //boolean fields, keep together to save some bytes
    FIsControl: Boolean;
    FShowHint: Boolean;
    FParentColor: Boolean;
    FParentFont: Boolean;
    FParentShowHint: Boolean;
    FAutoSize: Boolean;
    FAutoSizingAll: boolean;
    FAutoSizingSelf: Boolean;
    FEnabled: Boolean;
    FMouseInClient: boolean;
    FVisible: Boolean;
    function CaptureMouseButtonsIsStored: boolean;
    procedure DoActionChange(Sender: TObject);
    function GetAccessibleDescription: TCaption;
    function GetAccessibleValue: TCaption;
    function GetAccessibleRole: TLazAccessibilityRole;
    function GetAutoSizingAll: Boolean;
    function GetAnchorSide(Kind: TAnchorKind): TAnchorSide;
    function GetAnchoredControls(Index: integer): TControl;
    function GetBoundsRect: TRect;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetLRDockWidth: Integer;
    function GetTBDockHeight: Integer;
    function GetText: TCaption;
    function GetUndockHeight: Integer;
    function GetUndockWidth: Integer;
    function IsAnchorsStored: boolean;
    function IsBiDiModeStored: boolean;
    function IsEnabledStored: Boolean;
    function IsFontStored: Boolean;
    function IsHintStored: Boolean;
    function IsHelpContextStored: Boolean;
    function IsHelpKeyWordStored: boolean;
    function IsShowHintStored: Boolean;
    function IsVisibleStored: Boolean;
    procedure DoBeforeMouseMessage;
    procedure DoConstrainedResize(var NewLeft, NewTop, NewWidth, NewHeight: integer);
    procedure DoMouseDown(var Message: TLMMouse; Button: TMouseButton;
                          Shift: TShiftState);
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
    procedure SetAccessibleDescription(AValue: TCaption);
    procedure SetAccessibleValue(AValue: TCaption);
    procedure SetAccessibleRole(AValue: TLazAccessibilityRole);
    procedure SetAnchorSide(Kind: TAnchorKind; AValue: TAnchorSide);
    procedure SetBorderSpacing(const AValue: TControlBorderSpacing);
    procedure SetBoundsRect(const ARect: TRect);
    procedure SetBoundsRectForNewParent(const AValue: TRect);
    procedure SetClientHeight(Value: Integer);
    procedure SetClientSize(const Value: TPoint);
    procedure SetClientWidth(Value: Integer);
    procedure SetConstraints(const Value: TSizeConstraints);
    procedure SetDesktopFont(const AValue: Boolean);
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
    procedure SetWidth(Value: Integer);
  protected
    FAccessibleObject: TLazAccessibleObject;
    FControlState: TControlState;
    FCursor: TCursor;
    class procedure WSRegisterClass; override;
    function GetCursor: TCursor; virtual;
    procedure SetCursor(Value: TCursor); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    procedure DoOnParentHandleDestruction; virtual;
  protected
    // sizing/aligning
    procedure DoAutoSize; virtual;
    procedure DoAllAutoSize; virtual; // while autosize needed call DoAutoSize, used by AdjustSize and EnableAutoSizing
    procedure BeginAutoSizing; // set AutoSizing=true, can be used to prevent circles
    procedure EndAutoSizing;   // set AutoSizing=false
    procedure AnchorSideChanged(TheAnchorSide: TAnchorSide); virtual;
    procedure ForeignAnchorSideChanged(TheAnchorSide: TAnchorSide;
                                       Operation: TAnchorSideChangeOperation); virtual;
    procedure SetAlign(Value: TAlign); virtual;
    procedure SetAnchors(const AValue: TAnchors); virtual;
    procedure SetAutoSize(Value: Boolean); virtual;
    procedure BoundsChanged; virtual;
    function CreateControlBorderSpacing: TControlBorderSpacing; virtual;
    procedure DoConstraintsChange(Sender: TObject); virtual;
    procedure DoBorderSpacingChange(Sender: TObject;
                                    InnerSpaceChanged: Boolean); virtual;
    function IsBorderSpacingInnerBorderStored: Boolean; virtual;
    function IsCaptionStored: Boolean;
    procedure SendMoveSizeMessages(SizeChanged, PosChanged: boolean); virtual;
    procedure ConstrainedResize(var MinWidth, MinHeight,
                                MaxWidth, MaxHeight: TConstraintSize); virtual;
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: integer;
                         WithThemeSpace: Boolean); virtual;
    procedure DoOnResize; virtual;// call OnResize
    procedure DoOnChangeBounds; virtual;// call OnChangeBounds
    procedure CheckOnChangeBounds;// checks for changes and calls DoOnChangeBounds
    procedure Resize; virtual;// checks for changes and calls DoOnResize
    procedure RequestAlign; virtual;// smart calling Parent.AlignControls
    procedure UpdateAnchorRules;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer; KeepBase: boolean); virtual;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); virtual;
    procedure ScaleConstraints(Multiplier, Divider: Integer);
    procedure ChangeScale(Multiplier, Divider: Integer); virtual;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; virtual;
    procedure SetBiDiMode(AValue: TBiDiMode); virtual;
    procedure SetParentBiDiMode(AValue: Boolean); virtual;
    function IsAParentAligning: boolean;
    function GetClientOrigin: TPoint; virtual;
    function GetClientRect: TRect; virtual;// visual size of client area
    function GetLogicalClientRect: TRect; virtual;// logical size of client area (e.g. in a TScrollBox the logical client area can be bigger than the visual)
    function GetScrolledClientRect: TRect; virtual;// visual client area scrolled
    function GetClientScrollOffset: TPoint; virtual;
    function GetControlOrigin: TPoint; virtual;
    function IsClientHeightStored: boolean; virtual;
    function IsClientWidthStored: boolean; virtual;
    function WidthIsAnchored: boolean;
    function HeightIsAnchored: boolean;

    property AutoSizing: Boolean read FAutoSizingSelf;// see Begin/EndAutoSizing
    property AutoSizingAll: Boolean read GetAutoSizingAll;// set in DoAllAutoSize
    property AutoSizingLockCount: Integer read FAutoSizingLockCount; // in/decreased by Disable/EnableAutoSizing
  protected
    // protected messages
    procedure WMCancelMode(var Message: TLMessage); message LM_CANCELMODE;
    procedure WMContextMenu(var Message: TLMContextMenu); message LM_CONTEXTMENU;

    procedure WMLButtonDown(var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TLMRButtonDown); message LM_RBUTTONDOWN;
    procedure WMMButtonDown(var Message: TLMMButtonDown); message LM_MBUTTONDOWN;
    procedure WMXButtonDown(var Message: TLMXButtonDown); message LM_XBUTTONDOWN;
    procedure WMLButtonDBLCLK(var Message: TLMLButtonDblClk); message LM_LBUTTONDBLCLK;
    procedure WMRButtonDBLCLK(var Message: TLMRButtonDblClk); message LM_RBUTTONDBLCLK;
    procedure WMMButtonDBLCLK(var Message: TLMMButtonDblClk); message LM_MBUTTONDBLCLK;
    procedure WMXButtonDBLCLK(var Message: TLMXButtonDblClk); message LM_XBUTTONDBLCLK;
    procedure WMLButtonTripleCLK(var Message: TLMLButtonTripleClk); message LM_LBUTTONTRIPLECLK;
    procedure WMRButtonTripleCLK(var Message: TLMRButtonTripleClk); message LM_RBUTTONTRIPLECLK;
    procedure WMMButtonTripleCLK(var Message: TLMMButtonTripleClk); message LM_MBUTTONTRIPLECLK;
    procedure WMXButtonTripleCLK(var Message: TLMXButtonTripleClk); message LM_XBUTTONTRIPLECLK;
    procedure WMLButtonQuadCLK(var Message: TLMLButtonQuadClk); message LM_LBUTTONQUADCLK;
    procedure WMRButtonQuadCLK(var Message: TLMRButtonQuadClk); message LM_RBUTTONQUADCLK;
    procedure WMMButtonQuadCLK(var Message: TLMMButtonQuadClk); message LM_MBUTTONQUADCLK;
    procedure WMXButtonQuadCLK(var Message: TLMXButtonQuadClk); message LM_XBUTTONQUADCLK;
    procedure WMMouseMove(var Message: TLMMouseMove); message LM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
    procedure WMRButtonUp(var Message: TLMRButtonUp); message LM_RBUTTONUP;
    procedure WMMButtonUp(var Message: TLMMButtonUp); message LM_MBUTTONUP;
    procedure WMXButtonUp(var Message: TLMXButtonUp); message LM_XBUTTONUP;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure WMMouseHWheel(var Message: TLMMouseEvent); message LM_MOUSEHWHEEL;
    procedure WMMove(var Message: TLMMove); message LM_MOVE;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED;
    procedure LMCaptureChanged(var Message: TLMessage); message LM_CaptureChanged;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMSysFontChanged(var Message: TLMessage); message CM_SYSFONTCHANGED;
    procedure CMEnabledChanged(var Message: TLMEssage); message CM_ENABLEDCHANGED;
    procedure CMHitTest(var Message: TCMHittest) ; message CM_HITTEST;
    procedure CMMouseEnter(var Message :TLMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message :TLMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Message: TLMessage); message CM_HINTSHOW;
    procedure CMParentBiDiModeChanged(var Message: TLMessage); message CM_PARENTBIDIMODECHANGED;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CMParentFontChanged(var Message: TLMessage); message CM_PARENTFONTCHANGED;
    procedure CMParentShowHintChanged(var Message: TLMessage); message CM_PARENTSHOWHINTCHANGED;
    procedure CMVisibleChanged(var Message: TLMessage); message CM_VISIBLECHANGED;
    procedure CMTextChanged(var Message: TLMessage); message CM_TEXTCHANGED;
    procedure CMCursorChanged(var Message: TLMessage); message CM_CURSORCHANGED;
  protected
    // drag and drop
    procedure CalculateDockSizes;
    function CreateFloatingDockSite(const Bounds: TRect): TWinControl;
    function GetDockEdge(const MousePos: TPoint): TAlign; virtual;
    function GetDragImages: TDragImageList; virtual;
    function GetFloating: Boolean; virtual;
    function GetFloatingDockSiteClass: TWinControlClass; virtual;
    procedure BeforeDragStart; virtual;
    procedure BeginAutoDrag; virtual;
    procedure DoFloatMsg(ADockSource: TDragDockObject);virtual;//CM_FLOAT
    procedure DockTrackNoTarget(Source: TDragDockObject; X, Y: Integer); virtual;
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); virtual;
    function DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint; ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean):LRESULT; virtual;//Cm_Drag
    procedure DoEndDock(Target: TObject; X, Y: Integer); virtual;
    procedure DoEndDrag(Target: TObject; X,Y: Integer); virtual;
    procedure DoStartDock(var DragObject: TDragObject); virtual;
    procedure DoStartDrag(var DragObject: TDragObject); virtual;
    procedure DragCanceled; virtual;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState;
                       var Accept: Boolean); virtual;
    procedure PositionDockRect(DragDockObject: TDragDockObject); virtual;
    procedure SetDragMode(Value: TDragMode); virtual;
    function GetDefaultDockCaption: String; virtual;
    //procedure SendDockNotification; virtual; MG: probably not needed
  protected
    // key and mouse
    procedure Click; virtual;
    procedure DblClick; virtual;
    procedure TripleClick; virtual;
    procedure QuadClick; virtual;
    function GetMousePosFromMessage(const MessageMousePos: TSmallPoint): TPoint;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    function  DialogChar(var Message: TLMKey): boolean; virtual;
    procedure UpdateMouseCursor(X, Y: integer);
  protected
    procedure Changed;
    function  GetPalette: HPalette; virtual;
    function ChildClassAllowed(ChildClass: TClass): boolean; virtual;
    procedure ReadState(Reader: TReader); override; // called
    procedure Loaded; override;
    procedure LoadedAll; virtual; // called when all controls were Loaded and lost their csLoading
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
    procedure SetParentComponent(NewParentComponent: TComponent); override;
    procedure WndProc(var TheMessage: TLMessage); virtual;
    procedure ParentFormHandleInitialized; virtual; // called by ChildHandlesCreated of parent form
    function GetMouseCapture: Boolean; virtual;
    procedure CaptureChanged; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CanTab: Boolean; virtual;
    function GetDeviceContext(var WindowHandle: HWND): HDC; virtual;
    function GetEnabled: Boolean; virtual;
    function GetPopupMenu: TPopupMenu; virtual;
    procedure DoOnShowHint(HintInfo: PHintInfo); virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; virtual;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; virtual;
    function DoMouseWheelHorz(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; virtual;
    function DoMouseWheelLeft(Shift: TShiftState; MousePos: TPoint): Boolean; virtual;
    function DoMouseWheelRight(Shift: TShiftState; MousePos: TPoint): Boolean; virtual;
    procedure VisibleChanging; virtual;
    procedure VisibleChanged; virtual;
    procedure EnabledChanging; virtual;
    procedure EnabledChanged; virtual;
    procedure AddHandler(HandlerType: TControlHandlerType;
                         const AMethod: TMethod; AsFirst: boolean = false);
    procedure RemoveHandler(HandlerType: TControlHandlerType;
                            const AMethod: TMethod);
    procedure DoCallNotifyHandler(HandlerType: TControlHandlerType);
    procedure DoCallKeyEventHandler(HandlerType: TControlHandlerType;
                                    var Key: Word; Shift: TShiftState);
    procedure DoCallMouseWheelEventHandler(HandlerType: TControlHandlerType;
                                           Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
                                           var Handled: Boolean);
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); virtual;
    procedure SetZOrder(TopMost: Boolean); virtual;
    class function GetControlClassDefaultSize: TSize; virtual;
    function ColorIsStored: boolean; virtual;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); virtual;
    procedure DoFixDesignFontPPI(const AFont: TFont; const ADesignTimePPI: Integer);
    procedure DoScaleFontPPI(const AFont: TFont; const AToPPI: Integer; const AProportion: Double);
  protected
    // actions
    function GetActionLinkClass: TControlActionLinkClass; virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); virtual;
  protected
    // optional properties (not every descendent supports them)
    property ActionLink: TControlActionLink read FActionLink write FActionLink;
    property DesktopFont: Boolean read FDesktopFont write SetDesktopFont;
    property DragCursor: TCursor read FDragCursor write SetDragCursor default crDrag;
    property DragKind: TDragKind read FDragKind write FDragKind default dkDrag;
    property DragMode: TDragMode read FDragMode write SetDragMode default dmManual;
    property MouseCapture: Boolean read GetMouseCapture write SetMouseCapture;
    property ParentColor: Boolean read FParentColor write SetParentColor default True;
    property ParentFont: Boolean  read FParentFont write SetParentFont default True;
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
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseWheelDown: TMouseWheelUpDownEvent read FOnMouseWheelDown write FOnMouseWheelDown;
    property OnMouseWheelUp: TMouseWheelUpDownEvent read FOnMouseWheelUp write FOnMouseWheelUp;
    property OnMouseWheelHorz: TMouseWheelEvent read FOnMouseWheelHorz write FOnMouseWheelHorz;
    property OnMouseWheelLeft: TMouseWheelUpDownEvent read FOnMouseWheelLeft write FOnMouseWheelLeft;
    property OnMouseWheelRight: TMouseWheelUpDownEvent read FOnMouseWheelRight write FOnMouseWheelRight;
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
    procedure DragDrop(Source: TObject; X,Y: Integer); virtual;
    procedure Dock(NewDockSite: TWinControl; ARect: TRect); virtual;
    function ManualDock(NewDockSite: TWinControl;
                        DropControl: TControl = nil;
                        ControlSide: TAlign = alNone;
                        KeepDockSiteSize: Boolean = true): Boolean; virtual;
    function ManualFloat(TheScreenRect: TRect;
                         KeepDockSiteSize: Boolean = true): Boolean; virtual;
    function ReplaceDockedControl(Control: TControl; NewDockSite: TWinControl;
                           DropControl: TControl; ControlSide: TAlign): Boolean;
    function Dragging: Boolean;
    // accessibility
    function GetAccessibleObject: TLazAccessibleObject;
    function CreateAccessibleObject: TLazAccessibleObject; virtual;
    function GetSelectedChildAccessibleObject: TLazAccessibleObject; virtual;
    function GetChildAccessibleObjectAtPos(APos: TPoint): TLazAccessibleObject; virtual;
    //scale support
    function ScaleDesignToForm(const ASize: Integer): Integer;
    function ScaleFormToDesign(const ASize: Integer): Integer;
    function Scale96ToForm(const ASize: Integer): Integer;
    function ScaleFormTo96(const ASize: Integer): Integer;
    function Scale96ToFont(const ASize: Integer): Integer;
    function ScaleFontTo96(const ASize: Integer): Integer;
    function ScaleScreenToFont(const ASize: Integer): Integer;
    function ScaleFontToScreen(const ASize: Integer): Integer;
    function Scale96ToScreen(const ASize: Integer): Integer;
    function ScaleScreenTo96(const ASize: Integer): Integer;
  public
    // size
    procedure AdjustSize; virtual;// smart calling DoAutoSize
    function AutoSizePhases: TControlAutoSizePhases; virtual;
    function AutoSizeDelayed: boolean; virtual;
    function AutoSizeDelayedReport: string; virtual;
    function AutoSizeDelayedHandle: Boolean; virtual;
    procedure AnchorToNeighbour(Side: TAnchorKind; Space: TSpacingSize;
                                Sibling: TControl);
    procedure AnchorParallel(Side: TAnchorKind; Space: TSpacingSize;
                             Sibling: TControl);
    procedure AnchorHorizontalCenterTo(Sibling: TControl);
    procedure AnchorVerticalCenterTo(Sibling: TControl);
    procedure AnchorToCompanion(Side: TAnchorKind; Space: TSpacingSize;
                                Sibling: TControl;
                                FreeCompositeSide: boolean = true);
    procedure AnchorSame(Side: TAnchorKind; Sibling: TControl);
    procedure AnchorAsAlign(TheAlign: TAlign; Space: TSpacingSize);
    procedure AnchorClient(Space: TSpacingSize);
    function AnchoredControlCount: integer;
    property AnchoredControls[Index: integer]: TControl read GetAnchoredControls;
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer); virtual;
    procedure SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer
            ); virtual; // if you use this, disable the LCL autosizing for this control
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
                               Raw: boolean = false;
                               WithThemeSpace: boolean = true); virtual;
    function GetCanvasScaleFactor: Double;
    function GetDefaultWidth: integer;
    function GetDefaultHeight: integer;
    function GetDefaultColor(const DefaultColorType: TDefaultColorType): TColor; virtual;
    // These two are helper routines to help obtain the background color of a control
    function GetColorResolvingParent: TColor;
    function GetRGBColorResolvingParent: TColor;
    //
    function GetSidePosition(Side: TAnchorKind): integer;
    procedure CNPreferredSizeChanged;
    procedure InvalidatePreferredSize; virtual;
    function GetAnchorsDependingOnParent(WithNormalAnchors: Boolean): TAnchors;
    procedure DisableAutoSizing{$IFDEF DebugDisableAutoSizing}(const Reason: string){$ENDIF};
    procedure EnableAutoSizing{$IFDEF DebugDisableAutoSizing}(const Reason: string){$ENDIF};
    {$IFDEF DebugDisableAutoSizing}
    procedure WriteAutoSizeReasons(NotIfEmpty: boolean);
    {$ENDIF}
    procedure UpdateBaseBounds(StoreBounds, StoreParentClientSize,
                               UseLoadedValues: boolean); virtual;
    property BaseBounds: TRect read FBaseBounds;
    property ReadBounds: TRect read FReadBounds;
    property BaseParentClientSize: TSize read FBaseParentClientSize;
    procedure WriteLayoutDebugReport(const Prefix: string); virtual;
  public
    // LCL Scaling (High-DPI)
    procedure AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
      const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer); virtual;
    procedure ShouldAutoAdjust(var AWidth, AHeight: Boolean); virtual;
    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); virtual;
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); virtual;
  public
    constructor Create(TheOwner: TComponent);override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure EditingDone; virtual;
    procedure ExecuteDefaultAction; virtual;
    procedure ExecuteCancelAction; virtual;
    procedure BeginDrag(Immediate: Boolean; Threshold: Integer = -1);
    procedure EndDrag(Drop: Boolean);
    procedure BringToFront;
    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
    function IsParentOf(AControl: TControl): boolean; virtual;
    function GetTopParent: TControl;
    function IsVisible: Boolean; virtual;// checks parents too
    function IsControlVisible: Boolean; virtual;// does not check parents
    function IsEnabled: Boolean; // checks parent too
    function IsParentColor: Boolean; // checks protected ParentColor, needed by widgetsets
    function IsParentFont: Boolean; // checks protected ParentFont, needed by widgetsets
    function FormIsUpdating: boolean; virtual;
    function IsProcessingPaintMsg: boolean;
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
    function  Perform(Msg: Cardinal; WParam: WParam; LParam: LParam): LRESULT;
    function  ScreenToClient(const APoint: TPoint): TPoint; virtual;
    function  ClientToScreen(const APoint: TPoint): TPoint; virtual;
    function  ScreenToControl(const APoint: TPoint): TPoint;
    function  ControlToScreen(const APoint: TPoint): TPoint;
    function  ClientToParent(const Point: TPoint; AParent: TWinControl = nil): TPoint;
    function  ParentToClient(const Point: TPoint; AParent: TWinControl = nil): TPoint;
    function GetChildrenRect(Scrolled: boolean): TRect; virtual;
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
                                 AsFirst: boolean = false);
    procedure RemoveHandlerOnResize(const OnResizeEvent: TNotifyEvent);
    procedure AddHandlerOnChangeBounds(const OnChangeBoundsEvent: TNotifyEvent;
                                       AsFirst: boolean = false);
    procedure RemoveHandlerOnChangeBounds(const OnChangeBoundsEvent: TNotifyEvent);
    procedure AddHandlerOnVisibleChanging(const OnVisibleChangingEvent: TNotifyEvent;
                                          AsFirst: boolean = false);
    procedure RemoveHandlerOnVisibleChanging(const OnVisibleChangingEvent: TNotifyEvent);
    procedure AddHandlerOnVisibleChanged(const OnVisibleChangedEvent: TNotifyEvent;
                                         AsFirst: boolean = false);
    procedure RemoveHandlerOnVisibleChanged(const OnVisibleChangedEvent: TNotifyEvent);
    procedure AddHandlerOnEnabledChanged(const OnEnabledChangedEvent: TNotifyEvent;
                                         AsFirst: boolean = false);
    procedure RemoveHandlerOnEnableChanging(const OnEnableChangingEvent: TNotifyEvent);
    procedure AddHandlerOnKeyDown(const OnKeyDownEvent: TKeyEvent;
                                  AsFirst: boolean = false);
    procedure RemoveHandlerOnKeyDown(const OnKeyDownEvent: TKeyEvent);
    procedure AddHandlerOnBeforeDestruction(const OnBeforeDestructionEvent: TNotifyEvent;
                                  AsFirst: boolean = false);
    procedure RemoveHandlerOnBeforeDestruction(const OnBeforeDestructionEvent: TNotifyEvent);
    procedure AddHandlerOnMouseWheel(const OnMouseWheelEvent: TMouseWheelEvent;
                                  AsFirst: boolean = false);
    procedure RemoveHandlerOnMouseWheel(const OnMouseWheelEvent: TMouseWheelEvent);
  public
    // standard properties, which should be supported by all descendants
    property AccessibleDescription: TCaption read GetAccessibleDescription write SetAccessibleDescription;
    property AccessibleValue: TCaption read GetAccessibleValue write SetAccessibleValue;
    property AccessibleRole: TLazAccessibilityRole read GetAccessibleRole write SetAccessibleRole;
    property Action: TBasicAction read GetAction write SetAction;
    property Align: TAlign read FAlign write SetAlign default alNone;
    property Anchors: TAnchors read FAnchors write SetAnchors stored IsAnchorsStored default [akLeft, akTop];
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
    property Color: TColor read FColor write SetColor stored ColorIsStored default {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
    property Constraints: TSizeConstraints read FConstraints write SetConstraints;
    property ControlOrigin: TPoint read GetControlOrigin;
    property ControlState: TControlState read FControlState write FControlState;
    property ControlStyle: TControlStyle read FControlStyle write FControlStyle;
    property Enabled: Boolean read GetEnabled write SetEnabled stored IsEnabledStored default True;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property IsControl: Boolean read FIsControl write FIsControl;
    property MouseEntered: Boolean read FMouseInClient; deprecated 'use MouseInClient instead';// changed in 1.9, will be removed in 1.11
    property MouseInClient: Boolean read FMouseInClient;
    property OnChangeBounds: TNotifyEvent read FOnChangeBounds write FOnChangeBounds;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
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
  public
    function UseRightToLeftAlignment: Boolean; virtual;
    function UseRightToLeftReading: Boolean; virtual;
    function UseRightToLeftScrollBar: Boolean;
    function IsRightToLeft: Boolean;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored default bdLeftToRight;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
  published
    property AnchorSideLeft: TAnchorSide index akLeft read GetAnchorSide write SetAnchorSide;
    property AnchorSideTop: TAnchorSide index akTop read GetAnchorSide write SetAnchorSide;
    property AnchorSideRight: TAnchorSide index akRight read GetAnchorSide write SetAnchorSide;
    property AnchorSideBottom: TAnchorSide index akBottom read GetAnchorSide write SetAnchorSide;
    property Cursor: TCursor read GetCursor write SetCursor default crDefault;
    property Left: Integer read FLeft write SetLeft; // no default value - controls usually placed to different positions
    property Height: Integer read FHeight write SetHeight; // no default value - controls usually have different sizes
    property Hint: TTranslateString read FHint write SetHint stored IsHintStored;
    property Top: Integer read FTop write SetTop; // no default value - controls usually placed to different positions
    property Width: Integer read FWidth write SetWidth; // no default value - controls usually have different sizes
    property HelpType: THelpType read FHelpType write FHelpType default htContext;
    property HelpKeyword: String read FHelpKeyword write SetHelpKeyword stored IsHelpKeyWordStored;
    property HelpContext: THelpContext read FHelpContext write SetHelpContext stored IsHelpContextStored default 0;
  end;


  TBorderWidth = 0..MaxInt;

  TGetChildProc = procedure(Child: TComponent) of Object;

  { TControlChildSizing }

  { LeftRightSpacing, TopBottomSpacing: integer;
        minimum space between left client border and left most children.
        For example: ClientLeftRight=5 means children Left position is at least 5.

    HorizontalSpacing, VerticalSpacing: integer;
        minimum space between each child horizontally
  }

  {   Defines how child controls are resized/aligned.

      crsAnchorAligning
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
        Setting two children to Align=alClient does only make sense, if you set
        maximum Constraints.

        Order: First all alTop children are resized, then alBottom, then alLeft,
        then alRight and finally alClient.

      crsScaleChilds
        Scale children, keep space between them fixed.
        Children are resized to their normal/adviced size. If there is some space
        left in the client area of the parent, then the children are scaled to
        fill the space. You can set maximum Constraints. Then the other children
        are scaled more.
        For example: 3 child controls A, B, C with A.Width=10, B.Width=20 and
        C.Width=30 (total=60). If the Parent's client area has a ClientWidth of
        120, then the children are scaled with Factor 2.
        If B has a maximum constraint width of 30, then first the children will be
        scaled with 1.5 (A.Width=15, B.Width=30, C.Width=45). Then A and C
        (15+45=60 and 30 pixel space left) will be scaled by 1.5 again, to a
        final result of: A.Width=23, B.Width=30, C.Width=67 (23+30+67=120).

      crsHomogenousChildResize
        Enlarge children equally.
        Children are resized to their normal/adviced size. If there is some space
        left in the client area of the parent, then the remaining space is
        distributed equally to each child.
        For example: 3 child controls A, B, C with A.Width=10, B.Width=20 and
        C.Width=30 (total=60). If the Parent's client area has a ClientWidth of
        120, then 60/3=20 is added to each Child.
        If B has a maximum constraint width of 30, then first 10 is added to
        all children (A.Width=20, B.Width=30, C.Width=40). Then A and C
        (20+40=60 and 30 pixel space left) will get 30/2=15 additional,
        resulting in: A.Width=35, B.Width=30, C.Width=55 (35+30+55=120).

      crsHomogenousSpaceResize
        Enlarge space between children equally.
        Children are resized to their normal/adviced size. If there is some space
        left in the client area of the parent, then the space between the children
        is expanded.
        For example: 3 child controls A, B, C with A.Width=10, B.Width=20 and
        C.Width=30 (total=60). If the Parent's client area has a ClientWidth of
        120, then there will be 60/2=30 space between A and B and between
        B and C.

      crsSameSize - not implemented yet
        Set each child to the same size (maybe one pixel difference).
        The client area is divided by the number of controls and each control
        gets the same size. The remainder is distributed to the first children.
  }

  TChildControlResizeStyle = (
      crsAnchorAligning, // (like Delphi)
      crsScaleChilds, // scale children equally, keep space between children fixed
      crsHomogenousChildResize, // enlarge children equally (i.e. by the same amount of pixel)
      crsHomogenousSpaceResize // enlarge space between children equally
      {$IFDEF EnablecrsSameSize}
      ,crsSameSize  // each child gets the same size (maybe one pixel difference)
      {$ENDIF}
    );

  TControlChildrenLayout = (
      cclNone,
      cclLeftToRightThenTopToBottom, // if BiDiMode <> bdLeftToRight then it becomes RightToLeft
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
    procedure Change; virtual;
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
    property LeftRightSpacing: integer read FLeftRightSpacing write SetLeftRightSpacing default 0;
    property TopBottomSpacing: integer read FTopBottomSpacing write SetTopBottomSpacing default 0;
    property HorizontalSpacing: integer read FHorizontalSpacing write SetHorizontalSpacing default 0;
    property VerticalSpacing: integer read FVerticalSpacing write SetVerticalSpacing default 0;
    property EnlargeHorizontal: TChildControlResizeStyle read FEnlargeHorizontal
                           write SetEnlargeHorizontal default crsAnchorAligning;
    property EnlargeVertical: TChildControlResizeStyle read FEnlargeVertical
                             write SetEnlargeVertical default crsAnchorAligning;
    property ShrinkHorizontal: TChildControlResizeStyle read FShrinkHorizontal
                            write SetShrinkHorizontal default crsAnchorAligning;
    property ShrinkVertical: TChildControlResizeStyle read FShrinkVertical
                              write SetShrinkVertical default crsAnchorAligning;
    property Layout: TControlChildrenLayout read FLayout write SetLayout default cclNone;
    property ControlsPerLine: integer read FControlsPerLine write SetControlsPerLine default 0;
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
    wcfFontChanged,          // Set if font was changed before handle creation
    wcfAllAutoSizing,        // Set inside DoAllAutosize
    wcfAligningControls,
    wcfEraseBackground,
    wcfCreatingHandle,       // Set while constructing the handle of this control
    wcfInitializing,         // Set while initializing during handle creation
    wcfCreatingChildHandles, // Set while constructing the handles of the children
    wcfRealizingBounds,      // Set inside RealizeBoundsRecursive
    wcfBoundsRealized,       // bounds were sent to the interface
    wcfUpdateShowing,
    wcfHandleVisible,
    wcfAdjustedLogicalClientRectValid,
    wcfKillIntfSetBounds
    );
  TWinControlFlags = set of TWinControlFlag;

  TControlAtPosFlag = (
    capfAllowDisabled,   // include controls with Enabled=false
    capfAllowWinControls,// include TWinControls
    capfOnlyClientAreas, // use the client areas, not the whole child area
    capfRecursive,       // search recursively in grand childrens
    capfHasScrollOffset, // do not add the scroll offset to Pos (already included)
    capfOnlyWinControls  // include only TWinControls (ignore TControls)
    );
  TControlAtPosFlags = set of TControlAtPosFlag;

  // needed for VCL compatibility on custom aligning
  TAlignInfo = record
    AlignList: TFPList;    // The list of controls currently being aligned
    ControlIndex: Integer; // Index of current control
    Align: TAlign;         // The kind of alignment currently processed
                           // since this info is only used for custom aligning,
                           // the value is always alCustom
    Scratch: Integer;      // ??? Declared in the VCL, not used and not documented
  end;

  TAlignInsertBeforeEvent = function (Sender: TWinControl; Control1, Control2: TControl): Boolean of object;
  TAlignPositionEvent = procedure (Sender: TWinControl; Control: TControl;
                                   var NewLeft, NewTop, NewWidth, NewHeight: Integer;
                                   var AlignRect: TRect; AlignInfo: TAlignInfo) of object;

  { TWinControlEnumerator }

  TWinControlEnumerator = class
  protected
    FIndex: integer;
    FLowToHigh: boolean;
    FParent: TWinControl;
    function GetCurrent: TControl;
  public
    constructor Create(Parent: TWinControl; aLowToHigh: boolean = true);
    function GetEnumerator: TWinControlEnumerator;
    function MoveNext: Boolean;
    property Current: TControl read GetCurrent;
  end;

  TWinControl = class(TControl)
  private
    FAlignOrder: TFPList; // list of TControl. Last moved (SetBounds) comes first. Used by AlignControls.
    FBorderWidth: TBorderWidth;
    FBoundsLockCount: integer;
    FBoundsRealized: TRect;
    FBorderStyle: TBorderStyle;
    FBrush: TBrush;
    FAdjustClientRectRealized: TRect;
    FAdjustClientRect: TRect; // valid if wcfAdjustClientRectValid
    FChildSizing: TControlChildSizing;
    FControls: TFPList;    // the child controls
    FOnGetDockCaption: TGetDockCaptionEvent;
    FDefWndProc: Pointer;
    FDockClients: TFPList;
    FClientWidth: Integer;
    FClientHeight: Integer;
    FDockManager: TDockManager;
    FFlipped: boolean; // true if flipped - false if native
    FOnAlignInsertBefore: TAlignInsertBeforeEvent;
    FOnAlignPosition: TAlignPositionEvent;
    FOnDockDrop: TDockDropEvent;
    FOnDockOver: TDockOverEvent;
    FOnGetSiteInfo: TGetSiteInfoEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnUnDock: TUnDockEvent;
    FOnUTF8KeyPress: TUTF8KeyPressEvent;
    FParentDoubleBuffered: Boolean;
    FParentWindow: HWND;
    FRealizeBoundsLockCount: integer;
    FHandle: HWND;
    FTabOrder: integer;
    FTabList: TFPList;
    // keep small variables together to save some bytes
    FTabStop: Boolean;
    FShowing: Boolean;
    FDockSite: Boolean;
    FUseDockManager: Boolean;
    FDesignerDeleting: Boolean;
    procedure AlignControl(AControl: TControl);
    function DoubleBufferedIsStored: Boolean;
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
    procedure SetDoubleBuffered(Value: Boolean);
    procedure SetHandle(NewHandle: HWND);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetParentDoubleBuffered(Value: Boolean);
    procedure SetParentWindow(const AValue: HWND);
    procedure SetTabOrder(NewTabOrder: TTabOrder);
    procedure SetTabStop(NewTabStop: Boolean);
    procedure SetUseDockManager(const AValue: Boolean);
    procedure UpdateTabOrder(NewTabOrder: TTabOrder);
    procedure Insert(AControl: TControl);
    procedure Insert(AControl: TControl; Index: integer);
    procedure Remove(AControl: TControl);
    procedure AlignNonAlignedControls(ListOfControls: TFPList;
                                      var BoundsModified: Boolean);
    procedure CreateControlAlignList(TheAlign: TAlign;
                                    AlignList: TFPList; StartControl: TControl);
    procedure UpdateAlignIndex(aChild: TControl);
  protected
    FDoubleBuffered: Boolean;
    FWinControlFlags: TWinControlFlags;
    class procedure WSRegisterClass; override;
    procedure AdjustClientRect(var ARect: TRect); virtual;
    procedure GetAdjustedLogicalClientRect(out ARect: TRect);
    procedure AlignControls(AControl: TControl;
                            var RemainingClientRect: TRect); virtual;
    function CustomAlignInsertBefore(AControl1, AControl2: TControl): Boolean; virtual;
    procedure CustomAlignPosition(AControl: TControl; var ANewLeft, ANewTop, ANewWidth,
                                  ANewHeight: Integer; var AlignRect: TRect;
                                  AlignInfo: TAlignInfo); virtual;
    function DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
                     AControlList: TFPList; var ARect: TRect): Boolean; virtual;
    procedure DoChildSizingChange(Sender: TObject); virtual;
    procedure InvalidatePreferredChildSizes;
    function CanTab: Boolean; override;
    function IsClientHeightStored: boolean; override;
    function IsClientWidthStored: boolean; override;
    procedure DoSendShowHideToInterface; virtual; // called by TWinControl.CMShowingChanged
    procedure ControlsAligned; virtual;// called by AlignControls after aligning controls
    procedure DoSendBoundsToInterface; virtual; // called by RealizeBounds
    procedure RealizeBounds; virtual;// checks for changes and calls DoSendBoundsToInterface
    procedure RealizeBoundsRecursive; // called by DoAllAutoSize
    procedure InvalidateBoundsRealized;
    procedure CreateSubClass(var Params: TCreateParams; ControlClassName: PChar);
    procedure DoConstraintsChange(Sender: TObject); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure DoAutoSize; override;
    procedure DoAllAutoSize; override;
    procedure AllAutoSized; virtual; // called by DoAllAutoSize after all bounds are computed, see TCustomForm.AllAutoSized
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    procedure GetPreferredSizeClientFrame(out aWidth, aHeight: integer); virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    procedure PaintControls(DC: HDC; First: TControl);
    procedure PaintHandler(var TheMessage: TLMPaint);
    procedure PaintWindow(DC: HDC); virtual;
    procedure CreateBrush; virtual;
    procedure ScaleControls(Multiplier, Divider: Integer); virtual;
    procedure ChangeScale(Multiplier, Divider: Integer); override;
  protected
    // messages
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMBorderChanged(var Message: TLMessage); message CM_BORDERCHANGED;
    procedure CMDoubleBufferedChanged(var Message: TLMessage); message CM_DOUBLEBUFFEREDCHANGED;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMParentDoubleBufferedChanged(var Message: TLMessage); message CM_PARENTDOUBLEBUFFEREDCHANGED;
    procedure CMShowingChanged(var Message: TLMessage); message CM_SHOWINGCHANGED; // called by TWinControl.UpdateShowing
    procedure CMShowHintChanged(var Message: TLMessage); message CM_SHOWHINTCHANGED;
    procedure CMVisibleChanged(var Message: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnter(var Message: TLMessage); message CM_ENTER;
    procedure CMExit(var Message: TLMessage); message CM_EXIT;
    procedure WMContextMenu(var Message: TLMContextMenu); message LM_CONTEXTMENU;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMNotify(var Message: TLMNotify); message LM_NOTIFY;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMShowWindow(var Message: TLMShowWindow); message LM_SHOWWINDOW;
    procedure WMEnter(var Message: TLMEnter); message LM_ENTER;
    procedure WMExit(var Message: TLMExit); message LM_EXIT;
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
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;
    procedure CNKeyDown(var Message: TLMKeyDown); message CN_KEYDOWN;
    procedure CNSysKeyDown(var Message: TLMKeyDown); message CN_SYSKEYDOWN;
    procedure CNKeyUp(var Message: TLMKeyUp); message CN_KEYUP;
    procedure CNSysKeyUp(var Message: TLMKeyUp); message CN_SYSKEYUP;
    procedure CNChar(var Message: TLMKeyUp); message CN_CHAR;
  protected
    // drag and drop/dock
    function DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint;
                       ADragObject: TDragObject; ATarget:
                       TControl; ADocking: Boolean): LRESULT; override;
    function DoDockClientMsg(DragDockObject: TDragDockObject; aPosition: TPoint): boolean; virtual;
    function DoUndockClientMsg(NewTarget, Client: TControl):boolean; virtual;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); virtual;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
                       State: TDragState; var Accept: Boolean); virtual;
    procedure DoDockOver(Source: TDragDockObject; X, Y: Integer;
                         State: TDragState; var Accept: Boolean); virtual;
    procedure DoRemoveDockClient(Client: TControl); virtual;
    function  DoUnDock(NewTarget: TWinControl; Client: TControl;
                       KeepDockSiteSize: Boolean = true): Boolean; virtual;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
                          MousePos: TPoint; var CanDock: Boolean); virtual;
    function GetParentHandle: HWND;
    function GetTopParentHandle: HWND;
    procedure ReloadDockedControl(const AControlName: string;
                                  var AControl: TControl); virtual;
    function CreateDockManager: TDockManager; virtual;
    procedure SetDockManager(AMgr: TDockManager);
    procedure DoFloatMsg(ADockSource: TDragDockObject); override;//CM_FLOAT
    procedure DoGetDockCaption(AControl: TControl; var ACaption: String); virtual;
  protected
    // mouse and keyboard
    procedure DoEnter; virtual;
    procedure DoExit; virtual;
    function  DoKeyDownBeforeInterface(var Message: TLMKey; IsRecurseCall: Boolean): Boolean;
    function  DoRemainingKeyDown(var Message: TLMKeyDown): Boolean;
    function  DoRemainingKeyUp(var Message: TLMKeyDown): Boolean;
    function  DoKeyPress(var Message: TLMKey): Boolean;
    function  DoUTF8KeyPress(var UTF8Key: TUTF8Char): boolean; virtual;
    function  DoKeyUpBeforeInterface(var Message: TLMKey): Boolean;
    function  ChildKey(var Message: TLMKey): boolean; virtual;
    function  SendDialogChar(var Message: TLMKey): Boolean;
    function  DialogChar(var Message: TLMKey): boolean; override;
    procedure ControlKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure ControlKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyDownBeforeInterface(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyDownAfterInterface(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: char); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyUpBeforeInterface(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyUpAfterInterface(var Key: Word; Shift: TShiftState); virtual;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); virtual;
  protected
    function  FindNextControl(CurrentControl: TWinControl; GoForward,
                              CheckTabStop, CheckParent: Boolean): TWinControl;
    procedure SelectFirst;
    function  RealGetText: TCaption; override;
    function  GetBorderStyle: TBorderStyle;
    function  GetClientOrigin: TPoint; override;
    function  GetClientRect: TRect; override;
    function  GetControlOrigin: TPoint; override;
    function  GetDeviceContext(var WindowHandle: HWND): HDC; override;
    function GetParentBackground: Boolean;
    function  IsControlMouseMsg(var TheMessage): Boolean;
    procedure CreateHandle; virtual;
    procedure CreateParams(var Params: TCreateParams); virtual;
    procedure CreateWnd; virtual; //creates the window
    procedure DestroyHandle; virtual;
    procedure DestroyWnd; virtual;
    procedure DoFlipChildren; virtual;
    procedure FinalizeWnd; virtual; // gets called before the Handle is destroyed.
    procedure FixupTabList;
    procedure FontChanged(Sender: TObject); override;
    procedure InitializeWnd; virtual; // gets called after the Handle is created and before the missing child handles are created
    procedure Loaded; override;
    procedure FormEndUpdated; override;
    procedure MainWndProc(var Msg: TLMessage);
    procedure ParentFormHandleInitialized; override;
    procedure ChildHandlesCreated; virtual;// called after children handles are created
    function GetMouseCapture: Boolean; override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure RemoveFocus(Removing: Boolean);
    procedure SendMoveSizeMessages(SizeChanged, PosChanged: boolean); override;
    procedure SetBorderStyle(NewStyle: TBorderStyle); virtual;
    procedure SetColor(Value: TColor); override;
    procedure SetChildZPosition(const AChild: TControl; const APosition: Integer);
    procedure SetParentBackground(const AParentBackground: Boolean); virtual;
    procedure ShowControl(AControl: TControl); virtual;
    procedure UpdateControlState;
    procedure UpdateShowing; virtual; // checks control's handle visibility, called by DoAllAutoSize and UpdateControlState
    procedure WndProc(var Message: TLMessage); override;
    procedure WSSetText(const AText: String); virtual;
  protected
    property WindowHandle: HWND read FHandle write FHandle;
    // properties which are not supported by all descendents
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle default bsNone;
    property OnGetSiteInfo: TGetSiteInfoEvent read FOnGetSiteInfo write FOnGetSiteInfo;
    property OnGetDockCaption: TGetDockCaptionEvent read FOnGetDockCaption write FOnGetDockCaption;
    property ParentBackground: Boolean read GetParentBackground write SetParentBackground;
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
    property DockManager: TDockManager read FDockManager write SetDockManager;
    property DockSite: Boolean read FDockSite write SetDockSite default False;
    property DoubleBuffered: Boolean read FDoubleBuffered write SetDoubleBuffered stored DoubleBufferedIsStored;
    property Handle: HWND read GetHandle write SetHandle;
    property IsFlipped: Boolean read FFlipped;
    property IsResizing: Boolean read GetIsResizing;
    property TabOrder: TTabOrder read GetTabOrder write SetTabOrder default -1;
    property TabStop: Boolean read FTabStop write SetTabStop default false;
    property OnAlignInsertBefore: TAlignInsertBeforeEvent read FOnAlignInsertBefore write FOnAlignInsertBefore;
    property OnAlignPosition: TAlignPositionEvent read FOnAlignPosition write FOnAlignPosition;
    property OnDockDrop: TDockDropEvent read FOnDockDrop write FOnDockDrop;
    property OnDockOver: TDockOverEvent read FOnDockOver write FOnDockOver;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnUnDock: TUnDockEvent read FOnUnDock write FOnUnDock;
    property OnUTF8KeyPress: TUTF8KeyPressEvent read FOnUTF8KeyPress write FOnUTF8KeyPress;
    property ParentDoubleBuffered: Boolean read FParentDoubleBuffered write SetParentDoubleBuffered default True;
    property ParentWindow: HWND read FParentWindow write SetParentWindow;
    property Showing: Boolean read FShowing; // handle visible
    property UseDockManager: Boolean read FUseDockManager
                                     write SetUseDockManager default False;
    property DesignerDeleting: Boolean read FDesignerDeleting write FDesignerDeleting;
    property VisibleDockClientCount: Integer read GetVisibleDockClientCount;
  public
    // size, position, bounds
    function AutoSizePhases: TControlAutoSizePhases; override;
    function AutoSizeDelayed: boolean; override;
    function AutoSizeDelayedReport: string; override;
    function AutoSizeDelayedHandle: Boolean; override;
    procedure BeginUpdateBounds; // disable SetBounds
    procedure EndUpdateBounds;   // enable SetBounds
    procedure LockRealizeBounds; // disable sending bounds to widgetset
    procedure UnlockRealizeBounds; // enable sending bounds to widgetset, changes will now be sent
    function ControlAtPos(const Pos: TPoint; AllowDisabled: Boolean): TControl;
    function ControlAtPos(const Pos: TPoint;
                          AllowDisabled, AllowWinControls: Boolean): TControl;
    function ControlAtPos(const Pos: TPoint; Flags: TControlAtPosFlags): TControl; virtual;
    function  ContainsControl(Control: TControl): Boolean;
    procedure DoAdjustClientRectChange(const InvalidateRect: Boolean = True);
    procedure InvalidateClientRectCache(WithChildControls: boolean);
    function ClientRectNeedsInterfaceUpdate: boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    function  GetChildrenRect(Scrolled: boolean): TRect; override;
    procedure DisableAlign;
    procedure EnableAlign;
    procedure ReAlign; // realign all children
    procedure ScrollBy_WS(DeltaX, DeltaY: Integer);
    procedure ScrollBy(DeltaX, DeltaY: Integer); virtual;
    procedure WriteLayoutDebugReport(const Prefix: string); override;
    procedure AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy; const AFromPPI,
      AToPPI, AOldFormWidth, ANewFormWidth: Integer); override;
  public
    constructor Create(TheOwner: TComponent);override;
    constructor CreateParented(AParentWindow: HWND);
    class function CreateParentedControl(AParentWindow: HWND): TWinControl;
    destructor Destroy; override;
    procedure DockDrop(DragDockObject: TDragDockObject; X, Y: Integer); virtual;
    function CanFocus: Boolean; virtual;
    function CanSetFocus: Boolean; virtual;
    function GetControlIndex(AControl: TControl): integer;
    procedure SetControlIndex(AControl: TControl; NewIndex: integer);
    function Focused: Boolean; virtual;
    function PerformTab(ForwardTab: boolean): boolean; virtual;
    function FindChildControl(const ControlName: String): TControl;
    procedure SelectNext(CurControl: TWinControl;
                         GoForward, CheckTabStop: Boolean);
    procedure SetTempCursor(Value: TCursor); override;
    procedure BroadCast(var ToAllMessage);
    procedure NotifyControls(Msg: Word);
    procedure DefaultHandler(var AMessage); override;
    function  GetTextLen: Integer; override;
    procedure Invalidate; override;
    procedure AddControl; virtual; // tell widgetset

    procedure InsertControl(AControl: TControl);
    procedure InsertControl(AControl: TControl; Index: integer); virtual;
    procedure RemoveControl(AControl: TControl); virtual;
    // enumerators
    function GetEnumeratorControls: TWinControlEnumerator;
    function GetEnumeratorControlsReverse: TWinControlEnumerator;

    procedure Repaint; override;
    procedure Update; override;
    procedure SetFocus; virtual;
    procedure FlipChildren(AllLevels: Boolean); virtual;
    procedure ScaleBy(Multiplier, Divider: Integer);
    function GetDockCaption(AControl: TControl): String; virtual;
    procedure UpdateDockCaption(Exclude: TControl = nil); virtual;
    procedure GetTabOrderList(List: TFPList); virtual;
    function HandleAllocated: Boolean;
    function ParentHandlesAllocated: boolean; override;
    procedure HandleNeeded;
    function BrushCreated: Boolean;
    procedure EraseBackground(DC: HDC); virtual;
    function IntfUTF8KeyPress(var UTF8Key: TUTF8Char;
                              RepeatCount: integer; SystemKey: boolean): boolean; virtual;
    function IntfGetDropFilesTarget: TWinControl; virtual;
    procedure PaintTo(DC: HDC; X, Y: Integer); virtual; overload;
    procedure PaintTo(ACanvas: TCanvas; X, Y: Integer); overload;
    procedure SetShape(AShape: TBitmap); overload;
    procedure SetShape(AShape: TRegion); overload;
  end;


  { TGraphicControl }

  TGraphicControl = class(TControl)
  private
    FCanvas: TCanvas;
    FOnPaint: TNotifyEvent;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
  protected
    class procedure WSRegisterClass; override;
    procedure FontChanged(Sender: TObject); override;
    procedure Paint; virtual;
    procedure DoOnChangeBounds; override;
    procedure DoOnParentHandleDestruction; override;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    procedure CMCursorChanged(var Message: TLMessage); message CM_CURSORCHANGED;
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
    class procedure WSRegisterClass; override;
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure DestroyWnd; override;
    procedure PaintWindow(DC: HDC); override;
    procedure FontChanged(Sender: TObject); override;
    procedure SetColor(Value: TColor); override;
    procedure Paint; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property Scaled;
    property ShareImages;
    property Width;
    property OnChange;
    property OnGetWidthForPPI;
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
    FBounds: TRect;
  protected
    function GetHeight: Integer; virtual;
    function GetLeft: Integer; virtual;
    function GetLimitBegin: Integer; virtual;
    function GetLimitSize: Integer; virtual;
    function GetTop: Integer; virtual;
    function GetVisible: Boolean; virtual;
    function GetVisibleChildCount: Integer; virtual;
    function GetWidth: Integer; virtual;
    procedure SetLimitBegin(const AValue: Integer); virtual;
    procedure SetLimitSize(const AValue: Integer); virtual;
    procedure SetHeight(const AValue: Integer); virtual;
    procedure SetLeft(const AValue: Integer); virtual;
    procedure SetTop(const AValue: Integer); virtual;
    procedure SetWidth(const AValue: Integer); virtual;
  public
    constructor Create(TheTree: TDockTree; TheChildControl: TControl);
    function FindZone(AControl: TControl): TDockZone;
    function FirstVisibleChild: TDockZone;
    function GetNextVisibleZone: TDockZone;
    function NextVisible: TDockZone;
    function PrevVisible: TDockZone;
    procedure AddSibling(NewZone: TDockZone; InsertAt: TAlign);
    procedure AddAsFirstChild(NewChildZone: TDockZone);
    procedure AddAsLastChild(NewChildZone: TDockZone);
    procedure ReplaceChild(OldChild, NewChild: TDockZone);
    function GetLastChild: TDockZone;
    function GetIndex: Integer;
    procedure Remove(ChildZone: TDockZone);
  public
    property ChildControl: TControl read FChildControl;
    property ChildCount: Integer read FChildCount;
    property FirstChild: TDockZone read FFirstChildZone;
    property Height: Integer read GetHeight write SetHeight;
    property Left: Integer read GetLeft write SetLeft;
    property LimitBegin: Integer read GetLimitBegin write SetLimitBegin; // returns Left or Top
    property LimitSize: Integer read GetLimitSize write SetLimitSize;    // returns Width or Height
    property Orientation: TDockOrientation read FOrientation write FOrientation;
    property Parent: TDockZone read FParentZone;
    property Top: Integer read GetTop write SetTop;
    property Tree: TDockTree read FTree;
    property Visible: Boolean read GetVisible;
    property VisibleChildCount: Integer read GetVisibleChildCount;
    property Width: Integer read GetWidth write SetWidth;
    property NextSibling: TDockZone read FNextSibling;
    property PrevSibling: TDockZone read FPrevSibling;
  end;
  TDockZoneClass = class of TDockZone;


  { TDockTree - a tree of TDockZones - Every docked window has one tree

    This is an abstract class.
    A real implementation can be found for example in ldocktree.pas.

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
              And all three are children of the newly created TLazDockForm of the
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

    Every DockZone has siblings and children. Siblings can either be
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
    FFlags: TDockTreeFlags;
    FUpdateCount: Integer;
    procedure DeleteZone(Zone: TDockZone);
    procedure SetDockSite(const AValue: TWinControl);
  protected
    FRootZone: TDockZone;
    function HitTest(const MousePos: TPoint; var HTFlag: Integer): TControl; virtual;
    procedure PaintDockFrame(ACanvas: TCanvas; AControl: TControl;
                             const ARect: TRect); virtual;
    procedure UpdateAll;
    procedure SetDockZoneClass(const AValue: TDockZoneClass);
  public
    constructor Create(TheDockSite: TWinControl); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure AdjustDockRect(AControl: TControl; var ARect: TRect); virtual;
    procedure GetControlBounds(AControl: TControl;
                               out ControlBounds: TRect); override;
    procedure InsertControl(AControl: TControl; InsertAt: TAlign;
                            DropControl: TControl); override;
    procedure LoadFromStream(SrcStream: TStream); override;
    procedure MessageHandler(Sender: TControl; var Message: TLMessage); override;
    procedure PositionDockRect(AClient, DropCtl: TControl; DropAlign: TAlign;
                               var DockRect: TRect); override;
    procedure RemoveControl(AControl: TControl); override;
    procedure SaveToStream(DestStream: TStream); override;
    procedure SetReplacingControl(AControl: TControl); override;
    procedure ResetBounds(Force: Boolean); override;
    procedure PaintSite(DC: HDC); override;
    procedure DumpLayout(FileName: String); virtual;
  public
    property DockZoneClass: TDockZoneClass read FDockZoneClass;
    property DockSite: TWinControl read FDockSite write SetDockSite;
    property RootZone: TDockZone read FRootZone;
  end;

var
  DockSplitterClass: TControlClass = nil;

type
  { TMouse }

  TMouse = class
  private
    FWheelScrollLines: Integer;
    procedure SetCapture(const Value: HWND);
    function GetCapture: HWND;
    function GetCursorPos: TPoint;
    function GetIsDragging: Boolean;
    procedure SetCursorPos(AValue: TPoint);
    function GetWheelScrollLines: Integer;
    function GetDragImmediate: Boolean;
    procedure SetDragImmediate(const AValue: Boolean);
    function GetDragThreshold: Integer;
    procedure SetDragThreshold(const AValue: Integer);
  public
    property Capture: HWND read GetCapture write SetCapture;
    property CursorPos: TPoint read GetCursorPos write SetCursorPos;
    property IsDragging: Boolean read GetIsDragging;
    property WheelScrollLines: Integer read GetWheelScrollLines;
    property DragImmediate: Boolean read GetDragImmediate write SetDragImmediate;
    property DragThreshold: Integer read GetDragThreshold write SetDragThreshold;
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

function FindDragTarget(const Position: TPoint; AllowDisabled: Boolean): TControl;
function FindControlAtPosition(const Position: TPoint; AllowDisabled: Boolean): TControl;
function FindLCLWindow(const ScreenPos: TPoint; AllowDisabled: Boolean = True): TWinControl;
function FindControl(Handle: HWND): TWinControl;
function FindOwnerControl(Handle: HWND): TWinControl;
function FindLCLControl(const ScreenPos: TPoint): TControl;

function SendAppMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): Longint;
procedure MoveWindowOrg(dc: hdc; X,Y: Integer);

// Interface support.
procedure RecreateWnd(const AWinControl:TWinControl);


// drag and drop
var
  DefaultDockManagerClass: TDockManagerClass;

procedure CancelDrag;
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

procedure CheckTransparentWindow(var Handle: THandle; var AWinControl: TWinControl);
function CheckMouseButtonDownUp(const AWinHandle: THandle; const AWinControl: TWinControl;
  var LastMouse: TLastMouseInfo; const AMousePos: TPoint; const AButton: Byte;
  const AMouseDown: Boolean): Cardinal;

// shiftstate
function GetKeyShiftState: TShiftState;

procedure AdjustBorderSpace(var RemainingClientRect, CurBorderSpace: TRect;
  Left, Top, Right, Bottom: integer);
procedure AdjustBorderSpace(var RemainingClientRect, CurBorderSpace: TRect;
  const Space: TRect);

function IsColorDefault(AControl: TControl): Boolean;

function BidiFlipAlignment(Alignment: TAlignment; Flip: Boolean = True): TAlignment;
function BidiFlipAnchors(Control: TControl; Flip: Boolean): TAnchors;
function BidiFlipRect(const Rect: TRect; const ParentRect: TRect; const Flip: Boolean): TRect;
procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);

function DbgS(a: TAnchorKind): string; overload;
function DbgS(Anchors: TAnchors): string; overload;
function DbgS(a: TAlign): string; overload;
function DbgS(a: TAnchorKind; Side: TAnchorSideReference): string; overload;
function DbgS(p: TControlAutoSizePhase): string; overload;
function DbgS(Phases: TControlAutoSizePhases): string; overload;
function DbgS(cst: TControlStyleType): string; overload;
function DbgS(cs: TControlStyle): string; overload;

operator := (AVariant: Variant): TCaption;

function CompareLazAccessibleObjectsByDataObject(o1, o2: Pointer): integer;
function CompareDataObjectWithLazAccessibleObject(o, ao: Pointer): integer;

// register (called by the package initialization in design mode)
procedure Register;


implementation

uses
  WSControls, // circle with base widgetset is allowed
  WSLCLClasses,
  Forms, // the circle can't be broken without breaking Delphi compatibility
  Math;  // Math is in RTL and only a few functions are used.

var
  // The interface knows, which TWinControl has the capture. This stores
  // what child control of this TWinControl has actually the capture.
  CaptureControl: TControl=nil;

operator := (AVariant: Variant): TCaption;
begin
  Result := String(AVariant);
end;

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

function IsColorDefault(AControl: TControl): Boolean;
const
  NoDefaultValue = Longint($80000000);
var
  Info: PPropInfo;
begin
  Result := not AControl.ColorIsStored;
  if not Result then
  begin
    Info := GetPropInfo(AControl, 'Color');
    if Info <> nil then
      Result := (Info^.Default <> NoDefaultValue) and (Info^.Default = AControl.Color);
  end;
end;

function BidiFlipAlignment(Alignment: TAlignment; Flip: Boolean): TAlignment;
const
  BidiAlignment: array[Boolean, TAlignment] of TAlignment =
  (
    ( taLeftJustify, taRightJustify, taCenter ),
    ( taRightJustify, taLeftJustify, taCenter )
  );
begin
  Result := BidiAlignment[Flip, Alignment];
end;

function BidiFlipAnchors(Control: TControl; Flip: Boolean): TAnchors;
var
  LeftControl,RightControl : TControl;
  LeftSide,RightSide: TAnchorSideReference;
  NewAnchors: TAnchors;
begin
  Result := Control.Anchors;
  if Flip then
  begin
    LeftControl := Control.AnchorSide[akLeft].Control;
    LeftSide := Control.AnchorSide[akLeft].Side;
    if LeftSide = asrTop then LeftSide := asrBottom
    else if LeftSide = asrBottom then LeftSide := asrTop;

    RightControl := Control.AnchorSide[akRight].Control;
    RightSide := Control.AnchorSide[akRight].Side;
    if RightSide = asrTop then RightSide := asrBottom
    else if RightSide = asrBottom then RightSide := asrTop;

    Control.AnchorSide[akLeft].Control := RightControl;
    Control.AnchorSide[akLeft].Side := RightSide;
    Control.AnchorSide[akRight].Control := LeftControl;
    Control.AnchorSide[akRight].Side := LeftSide;

    NewAnchors := [];
    if (akTop in Result) then NewAnchors := NewAnchors + [akTop];
    if (akBottom in Result) then NewAnchors := NewAnchors + [akBottom];
    if (akLeft in Result) then NewAnchors := NewAnchors + [akRight];
    if (akRight in Result) then NewAnchors := NewAnchors + [akLeft];
    Result := NewAnchors;
  end;
end;

function BidiFlipRect(const Rect: TRect; const ParentRect: TRect; const Flip: Boolean): TRect;
var
  W: Integer;
begin
  Result := Rect;
  if Flip then
  begin
    W := Result.Right - Result.Left;
    Result.Left := ParentRect.Right - (Result.Left - ParentRect.Left) - W;
    Result.Right := Result.Left + W;
  end;
end;

procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);
begin
  case Alignment of
    taLeftJustify: Alignment := taRightJustify;
    taRightJustify: Alignment := taLeftJustify;
  end;
end;

function DbgS(a: TAnchorKind): string;
begin
  WriteStr(Result, a);
end;

function DbgS(Anchors: TAnchors): string;
var
  a: TAnchorKind;
begin
  Result:='';
  for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
    if a in Anchors then begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+DbgS(a);
    end;
  end;
  Result:='['+Result+']';
end;

function DbgS(a: TAlign): string;
begin
  WriteStr(Result, a);
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

function DbgS(p: TControlAutoSizePhase): string; overload;
begin
  WriteStr(Result, p);
end;

function DbgS(Phases: TControlAutoSizePhases): string; overload;
var
  p: TControlAutoSizePhase;
begin
  Result:='';
  for p:=Low(TControlAutoSizePhase) to High(TControlAutoSizePhase) do begin
    if p in Phases then begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+DbgS(p);
    end;
  end;
  Result:='['+Result+']';
end;

function DbgS(cst: TControlStyleType): string;
begin
  Result:='';
  WriteStr(Result,cst);
end;

function DbgS(cs: TControlStyle): string;
var
  cst: TControlStyleType;
begin
  Result:='';
  for cst:=low(TControlStyleType) to high(TControlStyleType) do
    if cst in cs then begin
      if Result<>'' then Result:=Result+',';
      Result:=Result+dbgs(cst);
    end;
  Result:='['+Result+']';
end;

function GetModalResultStr(ModalResult: TModalResult): ShortString;
begin
  Result := UITypes.ModalResultStr[ModalResult];
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
    {$IFNDEF DisableChecks}
    DebugLN('WARNING: obsolete call to RecreateWnd for %s', [AWinControl.ClassName]);
    {$ENDIF}
    //DumpStack;
  end;

  IsFocused := AWinControl.Focused;
  AWinControl.DestroyHandle;
  AWinControl.UpdateControlState;
  if IsFocused and AWinControl.HandleAllocated
  then SetFocus(AWinControl.FHandle);
end;

function CompareLazAccessibleObjectsByDataObject(o1, o2: Pointer): integer;
var
  AccObj1: TLazAccessibleObject absolute o1;
  AccObj2: TLazAccessibleObject absolute o2;
begin
  Result:=ComparePointers(AccObj1.DataObject,AccObj2.DataObject);
end;

function CompareDataObjectWithLazAccessibleObject(o, ao: Pointer): integer;
var
  AccObj: TLazAccessibleObject absolute ao;
begin
  Result:=ComparePointers(o,AccObj.DataObject);
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

procedure CheckTransparentWindow(var Handle: THandle; var AWinControl: TWinControl);
var
  NewFrm: TCustomForm;
  I: Integer;
  NewWinControl: TWinControl;
  LastFrm, NewFrmControl: TControl;
  MousePos: TPoint;
  MsgParam: LPARAM;
begin
  NewWinControl := AWinControl;
  MousePos := Mouse.CursorPos;
  MsgParam := MakeLParam(Word(MousePos.x), Word(MousePos.y));
  I := 0;
  while Assigned(NewWinControl)
  and (NewWinControl.Perform(LM_NCHITTEST, 0, MsgParam) = HTTRANSPARENT) do
  begin
    if NewWinControl.Parent=nil then
    begin // search underlying forms
      LastFrm := NewWinControl;
      NewWinControl := nil;
      while I < Screen.CustomFormZOrderCount do
      begin
        NewFrm := Screen.CustomFormsZOrdered[I];
        Inc(I);
        if (NewFrm<>NewWinControl)
        and PtInRect(NewFrm.BoundsRect, MousePos) then
        begin
          NewFrmControl := NewFrm.ControlAtPos(NewFrm.ScreenToClient(MousePos),
            [capfAllowWinControls, capfRecursive, capfOnlyWinControls]);
          if (NewFrmControl<>nil) and (NewFrmControl is TWinControl) then
            NewWinControl := TWinControl(NewFrmControl)
          else
            NewWinControl := NewFrm;
          Break;
        end;
      end;
    end else // search parent controls. todo (if really needed): search underlying controls within the same parent
      NewWinControl := NewWinControl.Parent;
  end;

  if NewWinControl<>nil then
  begin
    AWinControl := NewWinControl;
    Handle := AWinControl.Handle;
  end else
  begin
    // if no overlayed control was found, eat the message
    Handle := 0;
    AWinControl := nil;
  end;
end;

function CheckMouseButtonDownUp(const AWinHandle: THandle;
  const AWinControl: TWinControl; var LastMouse: TLastMouseInfo;
  const AMousePos: TPoint; const AButton: Byte; const AMouseDown: Boolean
  ): Cardinal;
const
  DblClickThreshold = 3;// max Movement between two clicks of a DblClick

  // array of clickcount x buttontype
  MSGKINDDOWN: array[1..4, 1..4] of Integer =
  (
    (LM_LBUTTONDOWN, LM_LBUTTONDBLCLK, LM_LBUTTONTRIPLECLK, LM_LBUTTONQUADCLK),
    (LM_RBUTTONDOWN, LM_RBUTTONDBLCLK, LM_RBUTTONTRIPLECLK, LM_RBUTTONQUADCLK),
    (LM_MBUTTONDOWN, LM_MBUTTONDBLCLK, LM_MBUTTONTRIPLECLK, LM_MBUTTONQUADCLK),
    (LM_XBUTTONDOWN, LM_XBUTTONDBLCLK, LM_XBUTTONTRIPLECLK, LM_XBUTTONQUADCLK)
  );
  MSGKINDUP: array[1..4] of Integer =
    (LM_LBUTTONUP, LM_RBUTTONUP, LM_MBUTTONUP, LM_XBUTTONUP);

  function LastClickInSameWinControl: boolean;
  begin
    Result := (LastMouse.WinHandle <> 0) and
              (LastMouse.WinHandle = AWinHandle) and
              (LastMouse.WinControl = AWinControl);
  end;

  function LastClickAtSamePosition: boolean;
  begin
    Result:= (Abs(AMousePos.X-LastMouse.MousePos.X) <= DblClickThreshold) and
             (Abs(AMousePos.Y-LastMouse.MousePos.Y) <= DblClickThreshold);
  end;

  function LastClickInTime: boolean;
  begin
    Result:=((GetTickCount64 - LastMouse.Time) <= GetDoubleClickTime);
  end;

  function LastClickSameButton: boolean;
  begin
    Result:=(AButton=LastMouse.Button);
  end;

  function TestIfMultiClickDown: boolean;
  begin
    Result:= LastClickInSameWinControl and
             LastClickAtSamePosition and
             LastClickInTime and
             LastClickSameButton;
  end;

  function TestIfMultiClickUp: boolean;
  begin
    Result:= LastClickInSameWinControl and
             LastClickAtSamePosition and
             LastClickSameButton;
  end;

var
  IsMultiClick: boolean;
  TargetControl: TControl;
  Button: Byte;
begin
  Result := LM_NULL;

  if AMouseDown then
    IsMultiClick := TestIfMultiClickDown
  else
    IsMultiClick := TestIfMultiClickUp;

  if AMouseDown then
  begin
    inc(LastMouse.ClickCount);

    if (LastMouse.ClickCount <= 4) and IsMultiClick then
    begin
      // multi click
    end else
    begin
      // normal click
      LastMouse.ClickCount:=1;
    end;

    LastMouse.Time := GetTickCount64;
    LastMouse.MousePos := AMousePos;
    LastMouse.WinControl := AWinControl;
    LastMouse.WinHandle := AWinHandle;
    LastMouse.Button := AButton;
  end else
  begin // mouse up
    if not IsMultiClick then
      LastMouse.ClickCount := 1;
  end;

  if (AWinControl<>nil) and not(csDesigning in AWinControl.ComponentState) then
  begin // runtime - handle multi clicks according to ControlStyle
    if LastMouse.ClickCount > 1 then
    begin
      TargetControl := AWinControl.ControlAtPos(AWinControl.ScreenToClient(AMousePos), [capfHasScrollOffset]);
      if TargetControl=nil then
        TargetControl := AWinControl;
      case LastMouse.ClickCount of
        2: if not(csDoubleClicks in TargetControl.ControlStyle) then LastMouse.ClickCount := 1;
        3: if not(csTripleClicks in TargetControl.ControlStyle) then LastMouse.ClickCount := 1;
        4: if not(csQuadClicks in TargetControl.ControlStyle) then LastMouse.ClickCount := 1;
      end;
    end;
  end else
  begin // design time or special system controls without TWinControl, allow only double clicks
    if LastMouse.ClickCount > 2 then
      LastMouse.ClickCount := 2;
  end;
  LastMouse.Down := AMouseDown;

  // mouse buttons 4,5 share same messages
  if AButton = 5 then
    Button := 4
  else
    Button := AButton;

  if AMouseDown then
    Result := MSGKINDDOWN[Button][LastMouse.ClickCount]
  else
    Result := MSGKINDUP[Button];
end;

function GetKeyShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
  if (GetKeyState(VK_LWIN) < 0) or (GetKeyState(VK_RWIN) < 0) then
    Include(Result, ssMeta);
end;

{------------------------------------------------------------------------------
  FindControl

  Returns the TWinControl associated with the Handle.
  This is very interface specific. Better use FindOwnerControl.

  Handle can also be a child handle, and does not need to be the Handle
  property of the Result.
  IMPORTANT: So, in most cases: Result.Handle <> Handle in the params.

------------------------------------------------------------------------------}
function FindControl(Handle: HWND): TWinControl;
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
function FindOwnerControl(Handle: HWND): TWinControl;
begin
  while Handle<>0 do
  begin
    Result := FindControl(Handle);
    if Result <> nil then
      Exit;
    Handle := GetParent(Handle);
  end;
  Result := nil;
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
  Result := nil;
  // find wincontrol at mouse cursor
  AWinControl := FindLCLWindow(ScreenPos);
  if AWinControl = nil then Exit;
  // find control at mouse cursor
  ClientPos := AWinControl.ScreenToClient(ScreenPos);
  Result := AWinControl.ControlAtPos(ClientPos,
                        [capfAllowDisabled, capfAllowWinControls, capfRecursive]);
  if Result = nil then
    Result := AWinControl;
end;

{-------------------------------------------------------------------------------
  function DoControlMsg(Handle: HWND; var Message): Boolean;

  Find the owner wincontrol and Perform the Message.
-------------------------------------------------------------------------------}
function DoControlMsg(Handle: HWND; var Message): Boolean;
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
function FindLCLWindow(const ScreenPos: TPoint; AllowDisabled: Boolean = True): TWinControl;
var
  Handle: HWND;
begin
  Handle := WindowFromPoint(ScreenPos);
  if not AllowDisabled then
    // if disabled windows are not allowed then go up and search first enabled window
    while IsWindow(Handle) and not IsWindowEnabled(Handle) do
      Handle := GetParent(Handle);

  if IsWindow(Handle) then
    Result := FindOwnerControl(Handle)
  else
    Result := nil;
end;

function FindDragTarget(const Position: TPoint; AllowDisabled: Boolean): TControl;
begin
  Result := FindControlAtPosition(Position, AllowDisabled);
end;

{------------------------------------------------------------------------------
  Function: FindControlAtPosition
  Params:
  Returns:

 ------------------------------------------------------------------------------}
function FindControlAtPosition(const Position: TPoint; AllowDisabled: Boolean): TControl;
const
  DisabledFlag: array[Boolean] of TControlAtPosFlags = ([], [capfAllowDisabled]);
var
  WinControl: TWinControl;
  Control: TControl;
begin
  Result := nil;
  WinControl := FindLCLWindow(Position, AllowDisabled);
  if Assigned(WinControl) then
  begin
    Result := WinControl;
    Control := WinControl.ControlAtPos(WinControl.ScreenToClient(Position),
                        [capfAllowWinControls, capfRecursive] + DisabledFlag[AllowDisabled]);
    //debugln(['FindControlAtPosition ',dbgs(Position),' ',DbgSName(WinControl),' ',dbgs(WinControl.ScreenToClient(Position)),' ',DbgSName(Control)]);
    if Assigned(Control) then
      Result := Control;
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

procedure CancelDrag;
begin
  if (DragManager <> nil) and DragManager.IsDragging then
    DragManager.DragStop(False);
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
  // OldCaptureWinControl: TWinControl;
  NewCaptureWinControl: TWinControl;
begin
  //DebugLn('SetCaptureControl Old=',DbgSName(CaptureControl),' New=',DbgSName(Control));
  if (CaptureControl=Control) then exit;

  if Control = nil then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('SetCaptureControl Only ReleaseCapture');
    {$ENDIF}
    // just unset the capturing, intf call not needed
    CaptureControl := nil;
    ReleaseCapture;
    Exit;
  end;

  // OldCaptureWinControl := FindOwnerControl(GetCapture);
  if Control is TWinControl then
    NewCaptureWinControl := TWinControl(Control)
  else
    NewCaptureWinControl := Control.Parent;

  if NewCaptureWinControl = nil then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLN('SetCaptureControl Only ReleaseCapture');
    {$ENDIF}
    // just unset the capturing, intf call not needed
    CaptureControl:=nil;
    ReleaseCapture;
    Exit;
  end;

  // Paul: don't uncomment. Intf call is needed since some widgetsets can install
  // capture themselves and release capture. Thus we can be in situation when we
  // get widgetset installed capture and don't install our own, later widgetset
  // releases its own capture and we have no capture. Such behavior was registered
  // on windows and it cased a bug #13615

// if NewCaptureWinControl = OldCaptureWinControl then
// begin
//  {$IFDEF VerboseMouseCapture}
//    DebugLN('SetCaptureControl Keep WinControl ',DbgSName(NewCaptureWinControl),
//    ' switch Control ',DbgSName(Control));
//  {$ENDIF}
//   CaptureControl := Control;
//   Exit;
// end;


  // switch capture control
  {$IFDEF VerboseMouseCapture}
  DebugLN('SetCaptureControl Switch to WinControl=',DbgSName(NewCaptureWinControl),
    ' and Control=',DbgSName(Control));
  {$ENDIF}
  CaptureControl := Control;
  ReleaseCapture;
  SetCapture(TWinControl(NewCaptureWinControl).Handle);
end;

{ Cursor translation function }

const
  DeadCursors = 1;

const
  CursorIdents: array[0..30] of TIdentMapEntry = (
    (Value: crDefault;      Name: 'crDefault'),
    (Value: crNone;         Name: 'crNone'),
    (Value: crArrow;        Name: 'crArrow'),
    (Value: crCross;        Name: 'crCross'),
    (Value: crIBeam;        Name: 'crIBeam'),
    (Value: crSizeNESW;     Name: 'crSizeNESW'),
    (Value: crSizeNS;       Name: 'crSizeNS'),
    (Value: crSizeNWSE;     Name: 'crSizeNWSE'),
    (Value: crSizeWE;       Name: 'crSizeWE'),
    (Value: crSizeNW;       Name: 'crSizeNW'),
    (Value: crSizeN;        Name: 'crSizeN'),
    (Value: crSizeNE;       Name: 'crSizeNE'),
    (Value: crSizeW;        Name: 'crSizeW'),
    (Value: crSizeE;        Name: 'crSizeE'),
    (Value: crSizeSW;       Name: 'crSizeSW'),
    (Value: crSizeS;        Name: 'crSizeS'),
    (Value: crSizeSE;       Name: 'crSizeSE'),
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
  for I := Low(CursorIdents) to High(CursorIdents) - DeadCursors do
    Proc(CursorIdents[I].Name);
end;

function CursorToIdent(Cursor: Longint; var Ident: string): Boolean;
begin
  Result := IntToIdent(Cursor, Ident, CursorIdents);
end;

function IdentToCursor(const Ident: string; var Cursor: Longint): Boolean;
begin
  Result := IdentToInt(Ident, Cursor, CursorIdents);
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

function TControlBorderSpacing.IsAroundStored: boolean;
begin
  if FDefault = nil
  then Result := FAround <> 0
  else Result := FAround <> FDefault^.Around;
end;

function TControlBorderSpacing.IsBottomStored: boolean;
begin
  if FDefault = nil
  then Result := FBottom <> 0
  else Result := FBottom <> FDefault^.Bottom;
end;

function TControlBorderSpacing.IsInnerBorderStored: boolean;
begin
  if Control <> nil then
    Result:=Control.IsBorderSpacingInnerBorderStored
  else
    Result:=True;
end;

function TControlBorderSpacing.IsLeftStored: boolean;
begin
  if FDefault = nil
  then Result := FLeft <> 0
  else Result := FLeft <> FDefault^.Left;
end;

function TControlBorderSpacing.IsRightStored: boolean;
begin
  if FDefault = nil
  then Result := FRight <> 0
  else Result := FRight <> FDefault^.Right;
end;

function TControlBorderSpacing.IsTopStored: boolean;
begin
  if FDefault = nil
  then Result := FTop <> 0
  else Result := FTop <> FDefault^.Top;
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

constructor TControlBorderSpacing.Create(OwnerControl: TControl; ADefault: PControlBorderSpacingDefault);
begin
  FControl := OwnerControl;
  FDefault := ADefault;
  if ADefault <> nil then
  begin
    FLeft := ADefault^.Left;
    FRight := ADefault^.Right;
    FTop := ADefault^.Top;
    FBottom := ADefault^.Bottom;
    FAround := ADefault^.Around;
  end;
  FCellAlignHorizontal := ccaFill;
  FCellAlignVertical := ccaFill;
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

procedure TControlBorderSpacing.AutoAdjustLayout(const AXProportion,
  AYProportion: Double);

  procedure Scale(var Value: Integer; const Proportion: Double; var Changed: Boolean);
  begin
    if Value<>0 then
    begin
      Value := Round(Value * Proportion);
      Changed := True;
    end;
  end;
var
  InnerChanged, OuterChanged: Boolean;
begin
  InnerChanged := False;
  OuterChanged := False;

  Scale(FAround, AXProportion, OuterChanged);
  Scale(FInnerBorder, AXProportion, InnerChanged);
  Scale(FLeft, AXProportion, OuterChanged);
  Scale(FTop, AYProportion, OuterChanged);
  Scale(FRight, AXProportion, OuterChanged);
  Scale(FBottom, AYProportion, OuterChanged);

  if OuterChanged or InnerChanged then
  begin
    if Control<>nil then Control.InvalidatePreferredSize;
    Change(InnerChanged);
  end;
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


function TControlBorderSpacing.GetSideSpace(Kind: TAnchorKind): Integer;
begin
  Result:=Around+GetSpace(Kind);
end;

function TControlBorderSpacing.GetSpace(Kind: TAnchorKind): Integer;
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
  if FControl <> nil then
    FControl.DoBorderSpacingChange(Self,InnerSpaceChanged);
  if Assigned(OnChange) then OnChange(Self);
end;

function TControlBorderSpacing.GetAroundBottom: Integer;
begin
  Result := Around+Bottom;
end;

function TControlBorderSpacing.GetAroundLeft: Integer;
begin
  Result := Around+Left;
end;

function TControlBorderSpacing.GetAroundRight: Integer;
begin
  Result := Around+Right;
end;

function TControlBorderSpacing.GetAroundTop: Integer;
begin
  Result := Around+Top;
end;

function TControlBorderSpacing.GetControlBottom: Integer;
begin
  if FControl<>nil then
    Result := FControl.Top+FControl.Height+Around+Bottom
  else
    Result := 0;
end;

function TControlBorderSpacing.GetControlHeight: Integer;
begin
  if FControl<>nil then
    Result := FControl.Height+Around*2+Top+Bottom
  else
    Result := 0;
end;

function TControlBorderSpacing.GetControlLeft: Integer;
begin
  if FControl<>nil then
    Result := FControl.Left-Around-Left
  else
    Result := 0;
end;

function TControlBorderSpacing.GetControlRight: Integer;
begin
  if FControl<>nil then
    Result := FControl.Left+FControl.Width+Around+Right
  else
    Result := 0;
end;

function TControlBorderSpacing.GetControlTop: Integer;
begin
  if FControl<>nil then
    Result := FControl.Top-Around-Top
  else
    Result := 0;
end;

function TControlBorderSpacing.GetControlWidth: Integer;
begin
  if FControl<>nil then
    Result := FControl.Width+Around*2+Left+Right
  else
    Result := 0;
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
  inherited Create;
  FControl := OwnerControl;
  FLayout := cclNone;
  FEnlargeHorizontal :=crsAnchorAligning;
  FEnlargeVertical := crsAnchorAligning;
  FShrinkHorizontal := crsAnchorAligning;
  FShrinkVertical := crsAnchorAligning;
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

  {$IFNDEF DisableChecks}
  procedure RaiseOwnerCircle;
  begin
    DebugLN('RaiseOwnerCircle AValue=',DbgSName(AValue),' FOwner=',DbgSName(FOwner));
    raise Exception.Create('TAnchorSide.SetControl AValue=FOwner');
  end;
  {$ENDIF}

var
  OldControl: TControl;
begin
  {$IFNDEF DisableChecks}
  if (AValue=FOwner) then RaiseOwnerCircle;
  {$ENDIF}
  if FControl=AValue then exit;
  OldControl:=FControl;
  if Side=asrCenter then begin
    FixCenterAnchoring;
    if Control<>OldControl then exit;
  end;
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
  FOwner.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TAnchorSide.SetSide'){$ENDIF};
  if AValue=asrCenter then begin
    OldSide:=FSide;
    FixCenterAnchoring;
    if OldSide<>FSide then exit;
  end;
  FSide:=AValue;
  FOwner.AnchorSideChanged(Self);
  if FControl<>nil then
    FControl.ForeignAnchorSideChanged(Self,ascoChangeSide);
  FOwner.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TAnchorSide.SetSide'){$ENDIF};
end;

function TAnchorSide.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

constructor TAnchorSide.Create(TheOwner: TControl; TheKind: TAnchorKind);
begin
  inherited Create;
  FOwner := TheOwner;
  FKind := TheKind;
  FSide := asrTop;
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

procedure TAnchorSide.GetSidePosition(out ReferenceControl: TControl; out
  ReferenceSide: TAnchorSideReference; out Position: Integer);
begin
  CheckSidePosition(Control,Side,ReferenceControl,ReferenceSide,Position);
end;

function TAnchorSide.CheckSidePosition(NewControl: TControl;
  NewSide: TAnchorSideReference;
  out ReferenceControl: TControl;
  out ReferenceSide: TAnchorSideReference; out Position: Integer): boolean;
{off $DEFINE VerboseAnchorSide}
var
  ParentRect: TRect;
  ParentRectValid: boolean;

  procedure RaiseInvalidSide;
  begin
    raise Exception.Create('TAnchorSide.CheckSidePosition invalid Side');
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

  function GetParentSidePos(Side: TAnchorKind): integer;
  begin
    if not ParentRectValid then begin
      FOwner.Parent.GetAdjustedLogicalClientRect(ParentRect);
      ParentRectValid:=true;
    end;
    case Side of
    akTop: Result:=ParentRect.Top;
    akLeft: Result:=ParentRect.Left;
    akRight: Result:=ParentRect.Right;
    akBottom: Result:=ParentRect.Bottom;
    end;
  end;

var
  NextReferenceSide: TAnchorSide;
  ChainLength: Integer;
  MaxChainLength: LongInt;
  OwnerBorderSpacing: LongInt;
  OwnerParent: TWinControl;
  Found: Boolean;
  CurReferenceControl: TControl;
  CurReferenceSide: TAnchorSideReference;
begin
  Result:=false;
  ReferenceControl:=nil;
  ReferenceSide:=Side;
  Position:=0;
  OwnerParent:=FOwner.Parent;
  if OwnerParent=nil then begin
    // AnchorSide is only between siblings or its direct parent allowed
    //if CheckPosition(Owner) then DebugLn(['TAnchorSide.GetSidePosition OwnerParent=nil']);
    exit;
  end;
  ParentRectValid:=false;
  ChainLength:=0;
  MaxChainLength:=OwnerParent.ControlCount;
  Found:=false;
  CurReferenceControl:=NewControl;
  CurReferenceSide:=NewSide;
  while CurReferenceControl<>nil do begin

    // check for circles
    if CurReferenceControl=Owner then begin
      // circle
      {$IFNDEF VerboseAnchorSide}
      DebugLn(['TAnchorSide.CheckSidePosition Circle, ',DbgSName(Owner),' ',dbgs(Kind)]);
      {$ENDIF}
      ReferenceControl:=nil;
      exit;
    end;

    inc(ChainLength);
    if ChainLength>MaxChainLength then begin
      // the chain has more elements than there are siblings -> circle
      //if CheckPosition(Owner) then
      {$IFNDEF VerboseAnchorSide}
      DebugLn(['TAnchorSide.CheckSidePosition Circle, ',DbgSName(Owner),' ',dbgs(Kind)]);
      {$ENDIF}
      ReferenceControl:=nil;
      exit;
    end;

    // check if ReferenceControl is valid
    if (CurReferenceControl.Parent<>OwnerParent)
    and (CurReferenceControl<>OwnerParent) then begin
      // not a sibling and not the parent -> invalid AnchorSide
      //if CheckPosition(Owner) then DebugLn(['TAnchorSide.GetSidePosition invalid AnchorSide ',dbgsName(ReferenceControl)]);
      {$IFNDEF VerboseAnchorSide}
      DebugLn(['TAnchorSide.CheckSidePosition invalid anchor control, ',DbgSName(Owner),' ',dbgs(Kind)]);
      {$ENDIF}
      ReferenceControl:=nil;
      exit;
    end;

    //debugln(['TAnchorSide.CheckSidePosition CurReferenceControl=',DbgSName(CurReferenceControl),' Kind=',dbgs(Kind),' Visible=',CurReferenceControl.IsControlVisible]);

    if CurReferenceControl.IsControlVisible then begin
      // ReferenceControl is visible
      if not Found then begin
        Found:=true;
        ReferenceControl:=CurReferenceControl;
        ReferenceSide:=CurReferenceSide;

        // -> calculate Position
        OwnerBorderSpacing:=FOwner.BorderSpacing.GetSideSpace(Kind);
        //if CheckPosition(Owner) then DebugLn(['TAnchorSide.CheckSidePosition ',dbgsName(Owner),' ReferenceControl=',dbgsName(ReferenceControl),' ',dbgs(ReferenceControl.BoundsRect),' OwnerBorderSpacing=',OwnerBorderSpacing,' Kind=',dbgs(Kind),' ReferenceSide=',dbgs(Kind,ReferenceSide)]);
        case ReferenceSide of

        asrTop: // asrTop = asrLeft
          if Kind in [akLeft,akRight] then begin
            // anchor to left side of ReferenceControl
            if ReferenceControl=OwnerParent then
              Position:=GetParentSidePos(akLeft)
            else
              Position:=ReferenceControl.Left;
            if ReferenceControl=OwnerParent then
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                      OwnerParent.ChildSizing.LeftRightSpacing)
            else if Kind=akRight then
              OwnerBorderSpacing:=Max(Max(OwnerBorderSpacing,
                   ReferenceControl.BorderSpacing.GetSideSpace(OppositeAnchor[Kind])),
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
              Position:=GetParentSidePos(akTop)
            else
              Position:=ReferenceControl.Top;
            if ReferenceControl=OwnerParent then
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                      OwnerParent.ChildSizing.TopBottomSpacing)
            else if Kind=akBottom then
              OwnerBorderSpacing:=Max(Max(OwnerBorderSpacing,
                   ReferenceControl.BorderSpacing.GetSideSpace(OppositeAnchor[Kind])),
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
              Position:=GetParentSidePos(akRight)
            else
              Position:=ReferenceControl.Left+ReferenceControl.Width;
            if ReferenceControl=OwnerParent then
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                      OwnerParent.ChildSizing.LeftRightSpacing)
            else if Kind=akLeft then
              OwnerBorderSpacing:=Max(Max(OwnerBorderSpacing,
                   ReferenceControl.BorderSpacing.GetSideSpace(OppositeAnchor[Kind])),
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
              Position:=GetParentSidePos(akBottom)
            else
              Position:=ReferenceControl.Top+ReferenceControl.Height;
            if ReferenceControl=OwnerParent then
              OwnerBorderSpacing:=Max(OwnerBorderSpacing,
                                      OwnerParent.ChildSizing.TopBottomSpacing)
            else if Kind=akTop then
              OwnerBorderSpacing:=Max(Max(OwnerBorderSpacing,
                   ReferenceControl.BorderSpacing.GetSideSpace(OppositeAnchor[Kind])),
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
              Position:=(GetParentSidePos(akRight)+GetParentSidePos(akLeft)) div 2
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
      end;
      // side found
      // continue to detect circles
    end;

    // try next
    NextReferenceSide:=nil;
    //debugln(['TAnchorSide.CheckSidePosition CurReferenceControl=',DbgSName(CurReferenceControl),' OwnerParent=',DbgSName(OwnerParent)]);
    if CurReferenceControl<>OwnerParent then
    begin
      // anchored to an invisible control
      //debugln(['TAnchorSide.CheckSidePosition skip invisible, try next CurReferenceControl=',DbgSName(CurReferenceControl),' Kind=',dbgs(Kind),' CurReferenceSide=',dbgs(Kind,CurReferenceSide)]);
      if CurReferenceSide=asrCenter then
      begin
        // center can only be anchored to another centered anchor
        if Kind in [akLeft,akRight] then
        begin
          if not GetNextCentered(CurReferenceControl,akLeft,NextReferenceSide)
          then   GetNextCentered(CurReferenceControl,akRight,NextReferenceSide);
        end else begin
          if not GetNextCentered(CurReferenceControl,akTop,NextReferenceSide)
          then   GetNextCentered(CurReferenceControl,akBottom,NextReferenceSide);
        end;
      end else if (CurReferenceSide=asrLeft) = (Kind in [akLeft,akTop]) then
      begin
        //debugln(['TAnchorSide.CheckSidePosition parallel CurReferenceControl=',DbgSName(CurReferenceControl),' Kind=',dbgs(Kind),' Anchors=',dbgs(CurReferenceControl.Anchors)]);
        // anchor parallel (e.g. a left side to a left side)
        if Kind in CurReferenceControl.Anchors then
          NextReferenceSide:=CurReferenceControl.AnchorSide[Kind]
        else if OppositeAnchor[Kind] in CurReferenceControl.Anchors then
          NextReferenceSide:=CurReferenceControl.AnchorSide[OppositeAnchor[Kind]];
      end else begin
        //debugln(['TAnchorSide.CheckSidePosition opposite CurReferenceControl=',DbgSName(CurReferenceControl),' Kind=',dbgs(Kind),' Anchors=',dbgs(CurReferenceControl.Anchors)]);
        // anchor opposite (e.g. a left side to a right side)
        if OppositeAnchor[Kind] in CurReferenceControl.Anchors then
          NextReferenceSide:=CurReferenceControl.AnchorSide[OppositeAnchor[Kind]]
        else if Kind in CurReferenceControl.Anchors then
          NextReferenceSide:=CurReferenceControl.AnchorSide[Kind];
      end;
    end;
    if (NextReferenceSide=nil) then
    begin
      // no further side => anchor ok
      // Note: if anchored control is not visible, it is anchored to the parent
      //if CheckPosition(Owner) and (Kind=akRight) then
      //if Owner.Name='ClassPartInsertPolicyRadioGroup' then
      //  DebugLn(['TAnchorSide.CheckSidePosition Success ',DbgSName(Owner),' ReferenceControl=',dbgsName(ReferenceControl),' CurReferenceControl=',DbgSName(CurReferenceControl),' CurReferenceSide=',dbgs(Kind,CurReferenceSide)]);
      exit(true);
    end;
    if NextReferenceSide=Self then begin
      CurReferenceControl:=NewControl;
      CurReferenceSide:=NewSide;
    end else begin
      CurReferenceControl:=NextReferenceSide.Control;
      CurReferenceSide:=NextReferenceSide.Side;
    end;
    //DebugLn(['TAnchorSide.CheckSidePosition ',DbgSName(FOwner),' ReferenceControl=',DbgSName(ReferenceControl),' Kind=',dbgs(Kind),' ReferenceSide=',dbgs(Kind,ReferenceSide)]);
  end;
  Result:=true;
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

function TAnchorSide.IsAnchoredToParent(ParentSide: TAnchorKind): boolean;
var
  ReferenceControl: TControl;
  ReferenceSide: TAnchorSideReference;
  p: Integer;
begin
  if (Owner.Align in [alClient,alLeft,alRight,alTop,alBottom])
  and (Kind in AnchorAlign[Owner.Align]) then
    exit(true); // aligned
  if not (Kind in Owner.Anchors) then
    exit(false); // not anchored
  GetSidePosition(ReferenceControl,ReferenceSide,p);
  if ReferenceControl=nil then
    exit(true); // default anchored to parent
  if Owner.Parent=nil then
    exit(false); // no parent
  if (ReferenceControl=Owner.Parent) and (Kind=ParentSide) then
    exit(true);
  Result:=false;
end;

procedure TAnchorSide.FixCenterAnchoring;
begin
  if (Side=asrCenter) and (Control<>nil) and (Kind in FOwner.Anchors) then
  begin
    // in case asrCenter, both sides are controlled by one anchor
    // -> disable opposite anchor and aligning
    if not (FOwner.Align in [alNone,alCustom]) then
      FOwner.Align:=alNone;
    FOwner.Anchors:=FOwner.Anchors-[OppositeAnchor[Kind]];
  end;
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

constructor TDragManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDragImmediate := True;
  FDragThreshold := 5;
end;

{ TDockManager }

procedure TDockManager.PositionDockRect(ADockObject: TDragDockObject);
begin
(* for now: defer to old PositionDockRect.
  Overridden methods should determine DropOnControl and DropAlign, before
    calling inherited method.
*)
  with ADockObject do
  begin
    if DropAlign = alNone then
    begin
      if DropOnControl <> nil then
        DropAlign := DropOnControl.GetDockEdge(DropOnControl.ScreenToClient(DragPos))
      else
        DropAlign := Control.GetDockEdge(DragTargetPos);
    end;
    PositionDockRect(Control, DropOnControl, DropAlign, FDockRect);
  end;
end;

procedure TDockManager.SetReplacingControl(Control: TControl);
begin

end;

function TDockManager.AutoFreeByControl: Boolean;
begin
  Result := True;
end;

constructor TDockManager.Create(ADockSite: TWinControl);
begin
  inherited Create;
end;

procedure TDockManager.BeginUpdate;
begin

end;

procedure TDockManager.EndUpdate;
begin

end;

function TDockManager.GetDockEdge(ADockObject: TDragDockObject): boolean;
begin
  { Determine the DropAlign.
    ADockObject contains valid DragTarget, DragPos, DragTargetPos relative
    dock site, and DropOnControl.
    Return True if ADockObject.DropAlign has been determined.
  }
  Result := False; // use the DockSite.GetDockEdge
end;

procedure TDockManager.InsertControl(ADockObject: TDragDockObject);
begin
  InsertControl(ADockObject.Control,ADockObject.DropAlign,
                ADockObject.DropOnControl);
end;

procedure TDockManager.PaintSite(DC: HDC);
begin

end;

procedure TDockManager.MessageHandler(Sender: TControl; var Message: TLMessage);
begin

end;

function TDockManager.IsEnabledControl(Control: TControl):Boolean;
begin
  Result := true;
  if Control is TWinControl then
    if (Control as TWinControl).DockManager <> nil then
      Result := (Control as TWinControl).DockManager = self;
end;


initialization
  //DebugLn('controls.pp - initialization');
  RegisterPropertyToSkip(TControl, 'AlignWithMargins', 'VCL compatibility property', '');
  RegisterPropertyToSkip(TControl, 'Ctl3D',            'VCL compatibility property', '');
  RegisterPropertyToSkip(TControl, 'ParentCtl3D',      'VCL compatibility property', '');
  RegisterPropertyToSkip(TControl, 'IsControl',        'VCL compatibility property', '');
  RegisterPropertyToSkip(TControl, 'DesignSize',       'VCL compatibility property', '');
  RegisterPropertyToSkip(TControl, 'ExplicitLeft',     'VCL compatibility property', '');
  RegisterPropertyToSkip(TControl, 'ExplicitHeight',   'VCL compatibility property', '');
  RegisterPropertyToSkip(TControl, 'ExplicitTop',      'VCL compatibility property', '');
  RegisterPropertyToSkip(TControl, 'ExplicitWidth',    'VCL compatibility property', '');
  {$IF FPC_FULLVERSION<30003}
  RegisterPropertyToSkip(TDataModule, 'PPI',    'PPI was introduced in FPC 3.0.3', '');
  {$ENDIF}
  Mouse := TMouse.Create;
  DefaultDockManagerClass := TDockTree;
  DragManager := TDragManagerDefault.Create(nil);
  RegisterIntegerConsts(TypeInfo(TCursor), @IdentToCursor, @CursorToIdent);

finalization
  FreeThenNil(DragManager);
  FreeThenNil(Mouse);

end.
